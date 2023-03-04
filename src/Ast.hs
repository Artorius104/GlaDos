module Ast(
        Ast(..),
        AssignedSymbols,
        evalAST,
        printMaybeAst
) where

import Data.Maybe()
import Data.Map(Map, lookup , insert, empty)

data Ast = Define String Ast
          | Lambda [String] Ast
          | If Ast Ast Ast
          | Number Int
          | Boolean Bool
          | Abstract String
          | ListAst [Ast]
          deriving Show

data AssignedValue = Variable Int
                    | Procedure [String] Ast
                    deriving Show
type AssignedSymbols = Map String AssignedValue

evalAST :: (Ast, AssignedSymbols) -> (Maybe Ast, AssignedSymbols)
evalAST (ast, symbs) = do
    let tmp = ast
    case tmp of
        Abstract abstract -> ((getAbstract abstract symbs), symbs)
        Number n ->  (Just (Number n), symbs)
        Boolean b -> (Just (Boolean b), symbs)
        ListAst [Abstract "+", a, b] -> (astMath (+) a b symbs, symbs)
        ListAst [Abstract "-", a, b] -> (astMath (-) a b symbs, symbs)
        ListAst [Abstract "*", a, b] -> (astMath (*) a b symbs, symbs)
        ListAst [Abstract "div", a, b] -> (astMath (div) a b symbs, symbs)
        ListAst [Abstract "mod", a, b] -> (astMath (mod) a b symbs, symbs)
        ListAst [Abstract "eq?", a, b] -> (astCmp (==) a b symbs, symbs)
        ListAst [Abstract ">", a, b] -> (astCmp (>) a b symbs, symbs)
        ListAst [Abstract "<", a, b] -> (astCmp (<) a b symbs, symbs)
        ListAst [Abstract ">=", a, b] -> (astCmp (>=) a b symbs, symbs)
        ListAst [Abstract "<=", a, b] -> (astCmp (<=) a b symbs, symbs)
        ListAst [Abstract "if", a, b, c] -> ((doIf a b c symbs), symbs)
        ListAst [Abstract "define", Abstract symb, Number a] -> (Just (Number a), (insert symb (Variable a) symbs))
        ListAst [Abstract "define", ListAst dec, eval] -> (tryAddFunction dec eval symbs)
        ListAst [Abstract "lambda", ListAst params, eval] ->  (tryMakeLambda params eval, symbs)
        ListAst (Lambda params eval:givenParams) -> (tryLambda params eval givenParams, symbs)
        ListAst (e:rest) -> do
            let (mEval, newSymbs) = (evalAST (e, symbs))
            case (mEval, rest) of
                (Nothing, _) -> (Nothing, newSymbs)
                (Just eval, (_: _)) -> (evalAST(ListAst ([eval] ++ rest), newSymbs))
                (Just eval, _) -> (Just eval, newSymbs)
        _ -> (Nothing, symbs)

astMath :: (Int -> Int -> Int) -> Ast -> Ast -> AssignedSymbols -> Maybe Ast
astMath op a1 a2 symbs = do
    let n1 = fst(evalAST (a1, symbs))
    let n2 = fst(evalAST (a2, symbs))
    case (n1, n2) of
        (Just (Number a), Just (Number b)) -> Just (Number (op a b))
        (_, _) -> Nothing

astCmp :: (Int -> Int -> Bool) -> Ast -> Ast -> AssignedSymbols -> Maybe Ast
astCmp op a1 a2 symbs = do
    let n1 = fst(evalAST (a1, symbs))
    let n2 = fst(evalAST (a2, symbs))
    case (n1, n2) of
        (Just (Number a), Just (Number b)) -> Just (Boolean (op a b))
        (_, _) -> Nothing

getAbstract :: String -> AssignedSymbols -> Maybe Ast
getAbstract key symbs = do
    let found = Data.Map.lookup key symbs
    case found of
        Just (Variable a) -> Just (Number a)
        Just (Procedure vars ast) -> Just (Lambda vars ast)
        Nothing -> Nothing

buildLambdaSymbols :: [String] -> [Int] -> AssignedSymbols
buildLambdaSymbols (symb:symbs) (value:values) = insert symb (Variable value) (buildLambdaSymbols symbs values)
buildLambdaSymbols _ _ = empty

doLambda :: [String] -> Ast -> [Int] -> Maybe Ast
doLambda lSymbs lEval params = let symbs = buildLambdaSymbols lSymbs params
    in fst(evalAST (lEval, symbs))

astsToStrings :: [Ast] -> Maybe [String]
astsToStrings [] = Just []
astsToStrings (ast:rest) = do
    let restArr = astsToStrings rest
    case (ast, restArr) of
        (Abstract symb, Just arr) -> Just ([symb] ++ arr)
        (_, _) -> Nothing

astsToInts :: [Ast] -> Maybe [Int]
astsToInts [] = Just []
astsToInts (ast:rest) = do
    let restArr = astsToInts rest
    case (ast, restArr) of
        (Number n, Just arr) -> Just ([n] ++ arr)
        (_, _) -> Nothing

tryLambda :: [String] -> Ast -> [Ast] -> Maybe Ast
tryLambda lSymbs lEval params = do
    let values = (astsToInts params)
    case values of
        Just v -> doLambda lSymbs lEval v
        _ -> Nothing

tryMakeLambda :: [Ast] -> Ast -> Maybe Ast
tryMakeLambda lSymbs lEval = do
    let strings = astsToStrings lSymbs
    case strings of
        Just a -> Just (Lambda a lEval)
        _ -> Nothing

tryAddFunction :: [Ast] -> Ast -> AssignedSymbols -> (Maybe Ast, AssignedSymbols)
tryAddFunction arr eval symbs = do
    let strings = astsToStrings arr
    case strings of
        Just (name:lSymbs) -> (Just (Number 0), insert name (Procedure lSymbs eval) symbs)
        _ -> (Nothing, symbs)

doIf :: Ast -> Ast -> Ast -> AssignedSymbols -> Maybe Ast
doIf cond a b symbs = do
    let evalCond = fst (evalAST (cond, symbs))
    let evalA = fst (evalAST (a, symbs))
    let evalB = fst (evalAST (b, symbs))
    case (evalCond, evalA, evalB) of
        (Just (Boolean True), Just n, _) -> Just n
        (Just (Boolean False), _, Just n) -> Just n
        (_, _, _) -> Nothing

maybeAstToString :: Maybe Ast -> String -> String
maybeAstToString ast errorMessage = do
    case ast of
        Just a -> show a
        Nothing -> errorMessage

printMaybeAst :: Maybe Ast -> String -> IO()
printMaybeAst ast errorMessage = putStrLn (maybeAstToString ast errorMessage)
