module Main (main) where

import Core(executeFile)
import System.Environment (getArgs)
import Control.Exception(try, SomeException, evaluate)
import Data.List(isPrefixOf)


checkArgs :: [String] -> [String] -> String -> ([String], String)
checkArgs [] [] _ = error "Invalid number of arguments: Not enougth arguments. Use ./glados -h for help."
checkArgs [] fileArr "" = (fileArr, "")
checkArgs [] fileArr name = (fileArr, name)
checkArgs (a:as) fileArr name = case a of
  "-o" -> case as of
    (n:bs) -> checkArgs bs fileArr n
    _ -> error "Inalid arguments. Use ./glados -h for help."
  _ -> checkArgs as (fileArr ++ [a]) name

printHelp :: IO()
printHelp = do
  putStrLn "Glados - a simple functional language"
  putStrLn "Usage: ./glados [options] [file]"
  putStrLn "Options:"
  putStrLn "  -o [name]  -  Set output file name"
  putStrLn "  -h         -  Print this help"

execute :: [String] -> IO()
execute [] = putStrLn "Execution failed"
execute args = do
    let (filepath, name) = checkArgs args [] ""
    if length filepath > 0 then
        case length filepath of
            1 -> if (head filepath) == "-h" then printHelp else (executeFile filepath name) >> putStr ""
            _ -> executeFile filepath name >> putStr ""
    else
        putStr ""



main :: IO ()
main = do
    args <- getArgs
    res <- try $ evaluate $ execute args :: IO (Either SomeException (IO ()))
    case res of
        Left err -> if (isPrefixOf "Empty input." (show err)) then return () else putStrLn $ "\x1b[31mError:\x1b[0m " ++ show err
        Right _ -> execute args
