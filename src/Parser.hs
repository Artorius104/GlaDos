module Parser(
        Ast(..),
        Parser,
        stringToAst,
        Name
) where

import Control.Applicative

import Lexer(Cpt(..), lexer)
type Name = String

data Ast
  = Float Double
  | Int Int
  | Boolean Bool
  | String String
  | Symbol String
  | Call Name [Ast]
  | Function Name [Ast] [Ast]
  | Extern Name Ast
  | BinaryOp Name Ast Ast
  | UnaryOp Name Ast
  | If Ast [Ast] [Ast]
  | While Ast [Ast]
  | For Ast Ast Ast [Ast]
  | Return Ast
  | Null
  deriving (Eq, Ord, Show)

newtype Parser a = Parser { runParser :: [Cpt] -> Maybe (a, [Cpt]) }

instance Functor Parser where
  fmap f (Parser g) = Parser (\cs -> fmap (\(x, cs') -> (f x, cs')) (g cs))


instance Applicative Parser where
  pure x = Parser (\cs -> Just (x, cs))
  Parser f <*> Parser g = Parser (\cs -> do
    (f', cs') <- f cs
    (x, cs'') <- g cs'
    return (f' x, cs''))

instance Monad Parser where
  return = pure
  Parser f >>= g = Parser (\cs -> do
    (x, cs') <- f cs
    runParser (g x) cs')

instance Alternative Parser where
  empty = Parser (\_ -> Nothing)
  Parser f <|> Parser g = Parser (\cs -> f cs <|> g cs)

--- While ---

getWhile :: Parser Ast
getWhile = Parser (\s -> case s of
    (LSymbol "while": List y: List z:xs) -> case runParser singleOperate y of
      Just (res, rest) -> case runParser operate z of
        Just (res2, rest) -> Just (While res res2, xs)
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing)

--- While ---

--- For ---

getForCondition :: [Cpt] -> Int -> [Ast] -> Maybe ([Ast], [Cpt])
getForCondition [] index res = if index == 2 then Just (res ++ [Null], []) else Just(res, [])
getForCondition s index res = case s of
  (Separator ';' : xs) -> if length res == 0 then getForCondition xs (index + 1) (res ++ [Null]) else getForCondition xs (index + 1) res
  (s) -> case runParser singleOperate s of
    Just (res2, rest) -> case checkForConditions res2 index res of
                          True -> getForCondition rest index (res ++ [res2])
                          False -> case index of
                            0 -> error "Invalid For arguments: verify your 1st argument."
                            1 -> error "Invalid For arguments: verify your 2nd argument."
                            2 -> error "Invalid For arguments: verify your 3rd argument."
                            _ -> error "Invalid For arguments: Too many arguments."
    _ -> Nothing

getFor :: Parser Ast
getFor = Parser (\s -> case s of
    (LSymbol "for": List y: List z:xs) -> case checkMultipleDefinitions y 0 ';' of
      True -> case getForCondition y 0 [] of
        Just (arr, []) -> case runParser operate z of
          Just (res, rest) -> if length arr == 3 then Just (For (arr !! 0) (arr !! 1) (arr !! 2) res, xs) else Just (For Null (arr !! 0) (arr !! 1) res, xs)
          _ -> Nothing
        _ -> Nothing
      False -> error "Invalid For arguments: Multiple definitions of \';\'."
    _ -> Nothing)

--- For ---

--- Function ---

getFuncConditions :: [Cpt] -> Int-> [Ast] -> Maybe ([Ast], [Cpt])
getFuncConditions [] index res = if length res == 0
                                 then Just (res, [])
                                 else if index == (length res) - 1
                                      then Just (res, [])
                                      else if length res == index
                                           then error "Invalid Function arguments: To much \',\' in function arguments."
                                           else error "Invalid Function arguments: \',\' need to be used for muliples arguments."
getFuncConditions s index res = case s of
  (Separator ',' : xs) -> getFuncConditions xs (index + 1) res
  (s) -> case runParser singleOperate s of
    Just (res2, rest) -> getFuncConditions rest index (res ++ [res2])
    _ -> Nothing

getFunction :: Parser Ast
getFunction = Parser (\s -> case s of
    (LSymbol "def": LSymbol x: List y: List z:xs) -> case checkMultipleDefinitions y 0 ',' of
      True -> case getFuncConditions y 0 [] of
        Just (res, rest) -> case runParser operate z of
          Just (res2, rest) -> Just (Function x res res2, xs)
          _ -> Nothing
        _ -> Nothing
      False -> error "Invalid Function arguments: Multiple definitions of \',\'."
    _ -> Nothing)

--- Function ---

--- Extern ---

getExtern :: Parser Ast
getExtern = Parser (\s -> case s of
    (LSymbol "extern": LSymbol x:xs) -> case runParser singleOperate xs of
      Just (res, rest) -> Just (Extern x res, rest)
      _ -> Nothing
    _ -> Nothing)             -- A revoir

--- Extern ---

--- String ---

getString :: Parser Ast
getString = Parser (\s -> case s of
    (LString x: xs) -> Just (String x, xs)
    _ -> Nothing)

--- String ---

--- Symbol ---

getSymbol :: Parser Ast
getSymbol = Parser (\s -> case s of
    (LSymbol x: xs) -> Just (Symbol x, xs)
    _ -> Nothing)

--- Symbol ---

--- If ---

getThenElse :: [Cpt] -> Maybe ([Ast], [Ast], [Cpt])
getThenElse t = case t of
  (List x: LSymbol "else":List y:xs) -> case runParser operate x of
    Just (res, rest) -> case runParser operate y of
      Just (res2, rest2) -> Just (res, res2, xs)
      _ -> Nothing
    _ -> Nothing
  (List x:xs) -> case runParser operate x of
    Just (res, rest) -> Just (res, [], xs)
    _ -> Nothing
  _ -> Nothing


getIf :: Parser Ast
getIf = Parser (\s -> case s of
    (LSymbol "if": List y:xs) -> case runParser singleOperate y of
      Just (res, rest) -> case getThenElse xs of
        Just (body1, body2, rest1) -> Just (If res body1 body2, rest1)
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing)

--- If ---

--- UnaryOp ---

getUnaryOp :: Parser Ast
getUnaryOp = Parser (\s -> case s of
    (Operator x: xs) -> case runParser singleOperate xs of
      Just (res, rest) -> if (isValidValue res) then Just (UnaryOp x res, rest) else Nothing
      _ -> Nothing
    (x: Operator y:xs) -> case runParser singleOperate [x] of
      Just (res, rest) -> if (isValidValue res) then Just (UnaryOp y res, xs) else Nothing
      _ -> Nothing
    _ -> Nothing)

-- UnaryOp ---

isValidValue :: Ast -> Bool
isValidValue (Int _) = True
isValidValue (Float _) = True
isValidValue (String _) = True
isValidValue (Symbol _) = True
isValidValue (BinaryOp _ _ _) = True
isValidValue (UnaryOp _ _) = True
isValidValue (Call _ _) = True
isValidValue (Function _ _ _) = True
isValidValue _ = False


--- BinaryOp ---

getValues :: [Cpt] -> [Cpt] -> Maybe ((Ast, Ast), [Cpt])
getValues x xs = case runParser singleOperate x of
      Just (res, rest) -> case runParser singleOperate xs of
        Just (res2, rest2) ->  Just ((res, res2), rest2)
        _ -> Nothing
      _ -> Nothing


getBinaryOp :: Parser Ast
getBinaryOp = Parser (\s -> case s of
    (List x: Operator y:List z:xs) -> case getValues x z of
      Just((res, res1), rest) -> Just (BinaryOp y res res1, xs)
      _ -> Nothing
    (x: Operator y:List z:xs) -> case getValues [x] z of
      Just((res, res1), rest) -> Just (BinaryOp y res res1, xs)
      _ -> Nothing
    (List x: Operator y:xs) -> case getValues x xs of
      Just((res, res1), rest) -> Just (BinaryOp y res res1, rest)
      _ -> Nothing
    (x: Operator y:xs) -> case getValues [x] xs of
      Just((res, res1), rest) -> Just (BinaryOp y res res1, rest)
      _ -> Nothing
    _ -> Nothing)
  
--- BinaryOp ---

--- CALL ---

getCall :: Parser Ast
getCall = Parser (\s -> case s of
  (LSymbol x: List y:xs) -> case checkMultipleDefinitions y 0 ',' of
    True -> case getFuncConditions y 0 [] of
      Just (res, rest) -> Just (Call x res, xs)
      _ -> Nothing
    False -> error "Invalid Call arguments: Multiple definitions of \',\'."
  _ -> Nothing)

--- CALL ----

--- RETURN ---

getReturn :: Parser Ast
getReturn = Parser (\s -> case s of
    (LSymbol "return": xs) -> case runParser singleOperate xs of
      Just (res, rest) -> Just (Return res, rest)
      _ -> Nothing
    _ -> Nothing)

--- RETURN ---

--- DOUBLE ---

getDouble :: Parser Ast
getDouble = Parser (\s -> case s of
    (Double n : xs) -> Just (Float n, xs)
    _ -> Nothing)

--- DOUBLE ---

--- INT ---

getNum :: Parser Ast
getNum = Parser (\s -> case s of
    (Num n : xs) -> Just (Int n, xs)
    _ -> Nothing)

--- INT ---

--- BOOL ---

getBool :: Parser Ast
getBool = Parser (\s -> case s of
    (LSymbol "true": xs) -> Just (Boolean True, xs)
    (LSymbol "false": xs) -> Just (Boolean False, xs)
    _ -> Nothing)

--- BOOL ---


singleOperate :: Parser Ast
singleOperate = (getFunction <|> getIf <|> getBinaryOp <|> getExtern <|> getUnaryOp <|> getFor <|> getWhile <|> getCall <|> getBool <|>  getNum <|> getDouble <|> getReturn <|> getString <|> getSymbol)

operate :: Parser [Ast]
operate = many (getFunction <|> getIf <|> getBinaryOp <|> getUnaryOp <|> getExtern <|> getFor <|> getWhile <|> getCall  <|> getBool <|>  getNum  <|> getDouble <|> getReturn <|> getString <|> getSymbol)

astParser :: [Cpt] -> [Ast]
astParser input = case runParser operate input of
  Just (tokens, []) -> tokens
  Just (_, remaining) -> error ("Can't parse the totality of the input.\n\tRemaining: " ++ show remaining ++ ".")
  Nothing -> error "Empty input."


cleanString :: String -> String
cleanString [] = []
cleanString (x:xs)
    | x == '\n' = cleanString xs
    | otherwise = x : cleanString xs


stringToAst :: String -> [Ast]
stringToAst s = case checkString (cleanString s) of
  Just a -> case lexer a of
    tokens -> case astParser tokens of
      ast -> if checkMain ast [] then ast else error "Invalid input: No entry point defined. You must define a main."
      _ -> []
    _ -> []
  Nothing -> []



---- Error Handling ----


---String Errors ---

checkString :: String -> Maybe String
checkString [] = error "Empty input."
checkString s = case getMatching s 0 of
  True -> Just s
  False -> Nothing

getMatching :: String -> Int -> Bool
getMatching [] index = if index == 0 then True else if index > 0 then error "Too many opening brackets." else error "Too many closing brackets."
getMatching (x:xs) index
  | x == '(' = getMatching xs (index + 1)
  | x == ')' = getMatching xs (index - 1)
  | otherwise = getMatching xs index


---String Errors ---


---Ast Errors ---

getMultipleMainDef :: [Ast]-> Int -> Int -> (Int, Int)
getMultipleMainDef [] funcIndex callIndex = if funcIndex == 0 && callIndex == 0
                                            then (-1, 0)
                                            else if funcIndex == 1 && callIndex == 1
                                                 then (0, 0)
                                                 else if funcIndex > callIndex
                                                      then (1, funcIndex)
                                                      else (2, callIndex)
getMultipleMainDef (x:xs) funcIndex callIndex = case x of
  Function "main" _ _ -> getMultipleMainDef xs (funcIndex + 1) callIndex
  Call "main" _ -> getMultipleMainDef xs funcIndex (callIndex + 1)
  _ -> getMultipleMainDef xs funcIndex callIndex

checkMain :: [Ast] -> [Ast] -> Bool
checkMain [] [] = False
checkMain [] res = case getMultipleMainDef res 0 0 of
  (-1, 0) -> error "Invalid input: No entry point defined. You must define a main."
  (0, 0) -> True
  (1, i) -> case i of
    1 -> error "Invalid input: No entry point called. main is defined but never called."
    _ -> error "Invalid input: Multiple entry points defined. You must define only one main."
  (2, j) -> case j of
    1 -> error "Invalid input: No entry point defined. You must define a main."
    _ -> error "Invalid input: Multiple call of main. Main can be called only once."
checkMain (x:xs) res = case x of
  Function "main" _ _ -> checkMain xs (x:res)
  Call "main" _ -> checkMain xs (x:res)
  _ -> checkMain xs res

checkForConditions :: Ast -> Int -> [Ast] -> Bool
checkForConditions a index arr = case index of
  0 -> case a of
    BinaryOp "=" _ _ -> True
    _ -> False
  1 -> True
  2 -> case a of 
    BinaryOp _ _ _ -> True
    UnaryOp _ _ -> True
    _ -> False
  _ -> False


checkMultipleDefinitions:: [Cpt] -> Int-> Char -> Bool
checkMultipleDefinitions [] _ _ = True
checkMultipleDefinitions (x:xs) index c = case x of
  Separator s -> if s == c then if index > 0 then False else checkMultipleDefinitions xs (index + 1) c else checkMultipleDefinitions xs index c
  _ -> if index > 0 then checkMultipleDefinitions xs (index - 1) c else checkMultipleDefinitions xs index c

---Ast Errors ---


---- Error Handling ----

