{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gen where

import Data.List
import Data.Function
import qualified Data.Map as Map
import Control.Monad.State

import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Short (toShort, fromShort)
import LLVM.AST.AddrSpace (AddrSpace(AddrSpace))
import qualified LLVM.AST.Float as C
import LLVM.IRBuilder (int32)
import LLVM.Internal.Value (typeOf)
import LLVM.Internal.DecodeAST (DecodeAST(unDecodeAST))

type SymbolTable = [(String, Operand)]

data CodegenState = CodegenState {
    currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name
  } deriving Show

data BlockState = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

type Names = Map.Map String Int


builtIn :: LLVM ()
builtIn = external (AST.IntegerType 32) "write"
  [ (AST.IntegerType 32, AST.Name "fd")
  , (AST.PointerType (AST.IntegerType 8) (AddrSpace 0), AST.Name "buf")
  , (AST.IntegerType 32, AST.Name "count")
  ]

opToString :: Operand -> String
opToString (LocalReference t name) = nameToString name
opToString (ConstantOperand c) = constToString c

nameToString :: Name -> String
nameToString (Name n) = B.unpack $ fromShort n

constToString :: C.Constant -> String
constToString (C.Int 32 i) = show i
constToString (C.Int 64 i) = show i
constToString (C.Float f) = show f
constToString (C.Float (C.Double d)) = show d
constToString (C.GlobalReference _ (Name n)) =  B.unpack $ fromShort n


write :: Operand -> Codegen Operand
write str = call (externf "write" VoidType) [AST.ConstantOperand (C.Int 32 1), str, int32 (toInteger (B.length (stringToByte (opToString str))))]

stringToByte :: String -> B.ByteString
stringToByte = B.pack

byteToString :: B.ByteString -> String
byteToString = B.unpack

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = toShort $ stringToByte label }


addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = AST.mkName label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

external ::  Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = AST.mkName label
  , linkage     = L.External
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = []
  }

double :: Type
double = FloatingPointType DoubleFP

string :: Type
string = PointerType (IntegerType 8) (AddrSpace 0)

int :: Type
int = IntegerType 32

void :: Type
void = VoidType

float :: Type
float = FloatingPointType FloatFP

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)


sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ show l

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (AST.mkName entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen


fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

instr :: Instruction -> Bool -> Codegen Operand
instr ins b = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = stack blk
  if not b
    then do
      modifyBlock (blk { stack = Do ins : i } )
      return $ local ref VoidType
    else do
      modifyBlock (blk { stack = (ref := ins) : i } )
      return $ local ref int


terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names

  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms

  modify $ \s -> s { blocks = Map.insert (AST.mkName qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (AST.mkName qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = (var, x) : lcls }


getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

local ::  Name -> Type -> Operand
local n t = LocalReference t n

global ::  Name -> Type -> C.Constant
global n t = C.GlobalReference t n

externf :: Name -> Type -> Operand
externf n t = ConstantOperand (C.GlobalReference t n)

fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr  (Add False False a b []) True

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr  (Sub False False a b []) True

fmul :: Operand -> Operand -> Codegen Operand
-- fmul a b "Float" = instr $ FMul noFastMathFlags a b []
fmul a b = instr (Mul False False a b []) True

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr  (SDiv False a b []) True


icmp ::  IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
icmp p a b = instr (ICmp p a b []) True

-- icmp :: IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
-- icmp pred a b = do
--   aType <- typeOf a
--   bType <- typeOf b
--   unless (aType == bType) $ error "Les deux op??randes ne sont pas de m??me type"
--   instr (ICmp pred a b [])


cons :: C.Constant -> Operand
cons = ConstantOperand

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr ( Call Nothing CC.C [] (Right fn) (toArgs args) [] []) True

alloca :: Type -> Codegen Operand
alloca ty = instr ( Alloca ty Nothing 0 [] ) True

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr ( Store False ptr val Nothing 0 []) False

load :: Operand -> Codegen Operand
load ptr = instr (Load False ptr Nothing 0 [] ) True

br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []
