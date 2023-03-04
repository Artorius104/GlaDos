{-# LANGUAGE OverloadedStrings #-}

module Op where

import Gen
import qualified Parser as P

import LLVM.Module
import LLVM.Context


import qualified LLVM.IRBuilder as B
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.FloatingPointPredicate as FP
import Control.Monad.Except
import qualified LLVM.AST.IntegerPredicate as IP
import qualified Data.Map as Map
import Data.ByteString.Short (toShort)
import Data.Functor.Contravariant (Op(Op))
import qualified LLVM.AST as AST.TypeKind

one = cons $ C.Float (F.Double 1.0)
zero = cons $ C.Float (F.Double 0.0)
false = zero
true = one

initModule :: AST.Module
initModule = emptyModule "entry"

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name (toShort $ stringToByte x)))

astStringToString :: P.Ast -> String
astStringToString (P.String s) = s
astStringToString (P.Symbol s) = s

codegenTop :: P.Ast -> LLVM ()
codegenTop (P.Function name args body) = do
  define double name fnargs bls
  where
    fnargs = [(double, AST.Name (toShort $ stringToByte $ astStringToString a)) | a <- args]
    -- fnargs = toSig $ map astStringToString args
    bls = createBlocks $ execCodegen $ do
      entr <- addBlock entryBlockName
      _ <- setBlock entr
      forM_ args $ \a -> do
        var <- alloca double
        _ <- store var (local (AST.Name (toShort $ stringToByte $ astStringToString a)) double)
        assign (astStringToString a) var
      mapM_ compileAst body

codegenTop (P.Extern name args) = do
  external double name fnargs
  where fnargs = toSig [astStringToString args]
codegenTop exp = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      _ <- setBlock entry
      compileAst exp >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> String -> Codegen AST.Operand
lt a b "Float" = do
  test <- fcmp FP.ULT a b
  uitofp double test
lt a b "Double" = do
  test <- fcmp FP.ULT a b
  uitofp double test
lt a b "Int" = do
  test <- instr $ AST.ICmp IP.ULT a b []
  uitofp int test
lt _ _ _ = error "Invalid type for comparison"

gt :: AST.Operand -> AST.Operand -> String -> Codegen AST.Operand
gt a b "Float" = do
  test <- fcmp FP.UGT a b
  uitofp float test
gt a b "Double" = do
  test <- fcmp FP.UGT a b
  uitofp double test
gt a b "Int" = do
  test <- instr $ AST.ICmp IP.UGT a b []
  uitofp int test
gt _ _ _ = error "Invalid type for comparison"


lte :: AST.Operand -> AST.Operand -> String -> Codegen AST.Operand
lte a b "Float" = do
  test <- fcmp FP.ULE a b
  uitofp double test
lte a b "Double" = do
  test <- fcmp FP.ULE a b
  uitofp double test
lte a b "Int" = do
  test <- instr $ AST.ICmp IP.ULE a b []
  uitofp int test
lte _ _ _ = error "Invalid type for comparison"

gte :: AST.Operand -> AST.Operand -> String -> Codegen AST.Operand
gte a b "Float" = do
  test <- fcmp FP.UGE a b
  uitofp double test
gte a b "Double" = do
  test <- fcmp FP.UGE a b
  uitofp double test
gte a b "Int" = do
  test <- instr $ AST.ICmp IP.UGE a b []
  uitofp int test
gte _ _ _ = error "Invalid type for comparison"

eq :: AST.Operand -> AST.Operand -> String -> Codegen AST.Operand
eq a b "Float" = do
  test <- fcmp FP.OEQ a b
  uitofp double test
eq a b "Double" = do
  test <- fcmp FP.OEQ a b
  uitofp double test
eq a b "Int" = do
  test <- instr $ AST.ICmp IP.EQ a b []
  uitofp int test
eq _ _ _ = error "Invalid type for comparison"

neq :: AST.Operand -> AST.Operand -> String -> Codegen AST.Operand
neq a b "Float" = do
  test <- fcmp FP.ONE a b
  uitofp double test
neq a b "Double" = do
  test <- fcmp FP.ONE a b
  uitofp double test
neq a b "Int" = do
  test <- instr $ AST.ICmp IP.NE a b []
  uitofp int test
neq _ _ _ = error "Invalid type for comparison"

fand :: AST.Operand -> AST.Operand -> String -> Codegen AST.Operand
fand a b _ = do
  test <- instr $ AST.ICmp IP.EQ a b []
  uitofp int test

ffor :: AST.Operand -> AST.Operand -> String -> Codegen AST.Operand
ffor a b _ = do
  test <- instr $ AST.ICmp IP.NE a b []
  uitofp int test


binops = Map.fromList [
      ("+", fadd)
    , ("-", fsub)
    , ("*", fmul)
    , ("/", fdiv)
    , ("<", lt)
    , ("==", eq)
    , ("!=", neq)
    , (">", gt)
    , (">=", gte)
    , ("<=", lte)
    , ("&&", fand)
    , ("||", ffor)
  ]

assignValue :: String -> P.Ast -> Codegen AST.Operand
assignValue var val = do
  a <- getvar var
  cval <- compileAst val
  _ <- store a cval
  return cval

applyOp :: AST.Operand -> AST.Operand -> P.Name -> String -> Codegen AST.Operand
applyOp a b op t = do
  case Map.lookup op binops of
    Just f -> f a b t
    Nothing -> error "Invalid operation"

makeOp :: P.Ast -> P.Ast -> P.Name -> Codegen AST.Operand
makeOp a b op = do
  case (a,  b) of
    (P.Int _, P.Int _) -> do
      ca <- compileAst a
      cb <- compileAst b
      applyOp ca cb op "Int"
    (P.Int _, P.Float _) -> do
      ca <- compileAst a
      cb <- compileAst b
      applyOp ca cb op "Double"
    (P.Float _, P.Int _) -> do
      ca <- compileAst a
      cb <- compileAst b
      applyOp ca cb op "Double"
    (P.Float _, P.Float _) -> do
      ca <- compileAst a
      cb <- compileAst b
      applyOp ca cb op "Double"
    (_,_) -> do
      ca <- compileAst a
      cb <- compileAst b
      applyOp ca cb op "Double"

compileAst :: P.Ast -> Codegen AST.Operand
compileAst (P.UnaryOp op a) = do
  compileAst $ P.Call ("unary" ++ op) [a]
compileAst (P.BinaryOp "=" (P.String var) val) = assignValue var val

compileAst (P.BinaryOp op a b) = makeOp a b op


-- compileAst (P.Call "print" args) = do
  -- let formatStrs = map getFormatStr args
  -- let printArgs = concatMap getPrintArgs args
  -- let printfFunc =  B.extern  "printf" [T.ptr T.i8]
  -- let formatStr = pure $ C.GlobalReference $ AST.Name "print_format_str"
  -- _ <- call printfFunc (formatStr : toArgs (map AST.ConstantOperand printArgs))
  -- return $ AST.ConstantOperand $ C.Float (F.Double 0.0)
  -- where
    -- getFormatStr :: P.Ast -> Maybe AST.Operand
    -- getFormatStr (P.Int _) = Just $ C.Array T.i8 $ map (C.Int 8 . toEnum . fromEnum) "%d"
    -- getFormatStr (P.Float _) = Just $ C.Array T.i8 $ map (C.Int 8 . toEnum . fromEnum) "%f"
    -- getFormatStr (P.String _) = Just $ C.Array T.i8 $ map (C.Int 8 . toEnum . fromEnum) "%s"
    -- getFormatStr _ = Nothing
    -- getPrintArgs :: P.Ast -> [AST.Operand]
    -- getPrintArgs (P.Int n) = [AST.ConstantOperand $ C.GetElementPtr True (C.GlobalReference (AST.Name "int_format_str")) [AST.ConstantOperand $ C.Int 32 0, AST.ConstantOperand $ C.Int 32 0], AST.ConstantOperand $ C.Int 32 (toInteger n)]
    -- getPrintArgs (P.Float f) = [AST.ConstantOperand $ C.GetElementPtr True (C.GlobalReference (AST.Name "float_format_str")) [AST.ConstantOperand $ C.Int 32 0, AST.ConstantOperand $ C.Int 32 0], AST.ConstantOperand $ C.Float $ F.Double f]
    -- getPrintArgs (P.String s) = [AST.ConstantOperand $ C.GetElementPtr True (C.GlobalReference (AST.Name "string_format_str")) [AST.ConstantOperand $ C.Int 32 0, AST.ConstantOperand $ C.Int 32 0], AST.ConstantOperand $ C.GlobalReference $ AST.Name s]
    -- getPrintArgs _ = []


compileAst (P.String x) = getvar x >>= load

compileAst (P.Float n) = return $ cons $ C.Float (F.Double n)

compileAst (P.Int n) = return $ cons $ C.Int 32 (toInteger n)

compileAst (P.Boolean b) = return $ if b then true else false

compileAst (P.Call fn args) = do
  largs <- mapM compileAst args
  call (externf (AST.Name (toShort $ stringToByte fn)) float ) largs

compileAst (P.If condExpr thenExprs elseExprs) = do
  thenB <- addBlock "if.then"
  elseB <- addBlock "if.else"
  endB <- addBlock "if.end"
  condOp <- compileAst condExpr
  condVal <- fcmp FP.ONE false condOp
  _ <- cbr condVal thenB elseB

  _ <- setBlock thenB
  mapM_ compileAst thenExprs
  _ <- br endB
  _ <- setBlock elseB
  mapM_ compileAst elseExprs
  _ <- br endB
  _ <- setBlock endB
  return $ AST.ConstantOperand $ C.Float (F.Double 0)

-- compileAst (P.For init cond update body) = do
--   loopHeader <- addBlock "loop.header"
--   loopBody <- addBlock "loop.body"
--   loopEnd <- addBlock "loop.end"
-- --    -- Évaluer l'expression d'initialisation et sauter au bloc du corps de la boucle
--   _ <- compileAst init
--   br loopBody
-- --    -- Bloc du corps de la boucle
--   emitBlockStart loopBody
--   mapM_ compileAst body
--   _ <- compileAst update
--   br loopHeader

-- --     -- Bloc de fin de boucle
--   setBlock loopEnd
--   return $ AST.ConstantOperand $ C.Int 32 0

-- --     where
-- --       newBlock name = do
-- --         fn <- gets currentFunction
-- --         let cnt = length $ AST.BasicBlock fn
-- --         let block = AST.BasicBlock (AST.Name $ name ++ "." ++ show cnt) [] (AST.Do $ AST.Br (AST.Name $ name ++ "." ++ show (cnt + 1)) [])
-- --         modify $ \s -> s { Codegen.currentBlock = Just block, blocks = block : blocks s }
-- --         return block

--   forloop <- addBlock "for.loop"
--   forexit <- addBlock "for.exit"
--   i <- alloca double
--   istart <- compileAst ivar
--   stepval <- compileAst step
--   store i istart
--   assign init i
--   br forloop

--   -- -- for.loop

--   setBlock forloop
--   mapM_ compileAst body
--   ival <- load i
--   inext <- fadd ival stepval
--   store i inext

--   cond <- compileAst cond
--   test <- fcmp FP.ONE false cond
--   cbr test forloop forexit

--   -- -- for.exit

--   setBlock forexit
--   return zero

compileAst (P.While cond body) = do
  condB <- addBlock "while.cond"
  bodyB <- addBlock "while.body"
  endB <- addBlock "while.end"
  _ <- br condB
  _ <- setBlock condB
  condOp <- compileAst cond
  condval <- fcmp FP.ONE false condOp
  _ <- cbr condval bodyB endB
  _ <- setBlock bodyB
  mapM_ compileAst body
  _ <- br condB
  _ <- setBlock endB
  return $ AST.ConstantOperand $ C.Int 32 0

compileAst (P.Symbol x) = getvar x >>= load

compileAst e = error $ "Not implemented: " ++ show e


liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [P.Ast] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  liftError $ liftIO $ withModuleFromAST context newast $ \m  -> do
    llstr <- moduleLLVMAssembly m
    writeFile "./LLVM.IR" (byteToString llstr)
    return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM mod modn

