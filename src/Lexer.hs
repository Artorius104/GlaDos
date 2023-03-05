module Lexer(
        Cpt(..),
        Lexer,
        lexer,
) where

import Control.Applicative

import Data.Word
import Data.Bits
import Text.Read(readMaybe)
import Data.List


data Cpt =  Num Int
         |  Double Double
         |  LSymbol String
         |  LString String
         |  Operator String
         |  Separator Char
         |  List [Cpt] 
         deriving (Show, Eq)

newtype Lexer a = Lexer { runLexer :: String -> Maybe (a, String) }

instance Functor Lexer where
  fmap f (Lexer g) = Lexer (\s -> fmap (\(x, s') -> (f x, s')) (g s))

instance Applicative Lexer where
  pure x = Lexer (\s -> Just (x, s))
  Lexer f <*> Lexer g = Lexer (\s -> do
    (f', s') <- f s
    (x, s'') <- g s'
    return (f' x, s''))

instance Monad Lexer where
  return = pure
  Lexer f >>= g = Lexer (\s -> do
    (x, s') <- f s
    runLexer (g x) s')

instance Alternative Lexer where
  empty = Lexer (\_ -> Nothing)
  Lexer f <|> Lexer g = Lexer (\s -> f s <|> g s)


--- LSymbols ---

parseSymbol :: Lexer Cpt
parseSymbol = Lexer (\s -> case parseAnySymbol s 0 "" of
    ("", _) -> Nothing
    (x, xs) -> Just (LSymbol x, xs))

isSymbol :: Char -> Bool
isSymbol c = c `elem` ['a'..'z'] || c `elem` ['A'..'Z']



parseAnySymbol :: String -> Int -> String -> (String, String)
parseAnySymbol [] _ res = (res, "")
parseAnySymbol (x:xs) index res = case isSymbol x of
          True -> parseAnySymbol xs index (res ++ [x])
          False -> (res, x:xs)

--- LSymbols ---

--- LString ---

parseString :: Lexer Cpt
parseString = Lexer (\s -> case parseAnyString s 0 "" of
    ("", _) -> Nothing
    (x, xs) -> Just (LString x, xs))

parseAnyString :: String -> Int -> String -> (String, String)
parseAnyString [] _ res = (res, "")
parseAnyString (x:xs) index res = case x of
  '\"' -> if index == 0 then parseAnyString xs (index + 1) res else parseAnyString xs (index - 1) res
  x -> if index > 0 then parseAnyString xs index (res ++ [x]) else (res, x:xs)

--- LString ---

--- Num ---

getNum :: Lexer Cpt
getNum = Lexer (\s -> case reads s :: [(Int, String)] of
  [(a, b)] -> if a > (minBound :: Int) && a < (maxBound :: Int) then Just (Num a, b) else Nothing
  _ -> Nothing)

--- Num ---

--- Double ---

getDouble :: Lexer Cpt
getDouble = Lexer (\s -> case reads s :: [(Double, String)] of
  [(a, b)] -> Just (Double a, b)
  _ -> Nothing)

--- Double ---

-- Operators --



parseOperators :: Lexer Cpt
parseOperators = Lexer (\s -> case parseAnyOpe s "" of
    ("", _) -> Nothing
    (x, xs) -> Just (Operator x, xs))

parseAnyOpe :: String -> String -> (String, String)
parseAnyOpe [] res = (res, "")
parseAnyOpe (x:xs) res = case isOperator x of
  True -> parseAnyOpe xs (res ++ [x])
  False -> (res, x:xs)

isOperator :: Char -> Bool
isOperator c = c `elem` ['+', '-', '*', '/', '^', '>', '<', '=', '&', '|', '!', '%']

-- Operators --

---Separators ----

parseSeparators :: Lexer Cpt
parseSeparators = Lexer (\s -> case s of
    (x:xs) -> if isSeparator x then Just (Separator x, xs) else Nothing
    _ -> Nothing)

isSeparator :: Char -> Bool
isSeparator c = c `elem` ['(', ')', '[', ']', '{', '}', ',', ';', ':', '.', ' ']

---Separators ----

---- List ----

parseList :: Lexer Cpt
parseList = getParsedList ('(', ')') <|> getParsedList ('{', '}')

getParsedList :: (Char, Char) -> Lexer Cpt
getParsedList delim = Lexer (\s -> let maybeListStr = splitFirstList s delim
    in case maybeListStr of
      Nothing -> Nothing
      Just (listStr, rest) -> Just (List (lexer listStr), rest)
  ) 

splitFirstList :: String -> (Char, Char) -> Maybe (String, String)
splitFirstList (fst:rest) (start, stop) = if fst == start 
  then splitAtEndOfBlock ([fst] ++ rest) (start, stop) 
  else Nothing
splitFirstList _ _ = Nothing

splitAtEndOfBlock :: String -> (Char, Char) -> Maybe (String, String)
splitAtEndOfBlock str delim = splitAtEndOfBlockRec str delim 0 ""

splitAtEndOfBlockRec :: String -> (Char, Char) -> Int -> String -> Maybe (String, String)
splitAtEndOfBlockRec (fst:rest) (start, stop) count progress
  | fst == stop && count == 1 = Just (tail progress, rest)
  | fst == stop = splitAtEndOfBlockRec rest (start, stop) (count - 1) (progress ++ [fst])
  | fst == start = splitAtEndOfBlockRec rest (start, stop) (count + 1) (progress ++ [fst])
  | otherwise = splitAtEndOfBlockRec rest (start, stop) count (progress ++ [fst])
splitAtEndOfBlockRec _ _ _ _ = Nothing

testPattern :: String -> Int
testPattern (a:b) = 5
testPattern _ = 3

splitAtChar :: String -> Char -> Maybe (String, String)
splitAtChar str end = 
  let findIdx = elemIndex end str
  in case findIdx of
    Nothing -> Nothing
    Just n -> let (fst, snd) = splitAt (n + 1) str in
      Just (init fst, snd)

---- List ----

cleanWhiteSpaces :: [Cpt] -> [Cpt]
cleanWhiteSpaces [] = []
cleanWhiteSpaces (x:xs)
    | x == Separator ' ' = cleanWhiteSpaces xs
    | otherwise = x:cleanWhiteSpaces xs

operate :: Lexer [Cpt]
operate = many (parseList <|> parseSymbol <|> parseOperators <|> getNum <|> getDouble  <|> parseSeparators <|> parseString)



lexer :: String -> [Cpt]
lexer input = case runLexer operate input of
  Just (tokens, "") -> cleanWhiteSpaces tokens
  Just (_, (r:rs)) -> error ("\'" ++ [r] ++ "\' has no reference and cannot be used in this language.")
  Nothing -> error "Lexer failed. Empty input."
