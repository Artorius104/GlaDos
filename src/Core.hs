module Core(
    mainLoop,
    executeFile,
    exe
) where


import Ast(AssignedSymbols, printMaybeAst, evalAST,)
import Parser(Ast(..), Parser, stringToAst)
import Data.Char()
import Data.Maybe()
import Data.Map(empty)
import Op
import Gen
import Data.Typeable(typeOf)
import Control.Exception(try, SomeException, evaluate)
import Data.List(isPrefixOf)


mainLoop :: IO ()
mainLoop = loopAst empty

loopAst :: AssignedSymbols -> IO()
loopAst symbs = do
    input <- getLine
    if input == "quit" || input == "exit"
        then return ()
        else do
            putStrLn "Parsed: "
            let parse = Nothing
            printMaybeAst parse "Error in parsing"
            putStrLn "Executed: "
            case parse of
                Nothing -> loopAst symbs
                Just ast -> do
                    let (res, newSymbs) = evalAST (ast, symbs)
                    printMaybeAst res "Error in execution"
                    loopAst newSymbs

executeLines :: [String] -> AssignedSymbols -> IO()
executeLines (line:rest) symbs = do
    let maybeAst = Nothing
    case maybeAst of
        Just ast -> do
            let (res, newSimbs) = evalAST (ast, symbs)
            printMaybeAst res "Error in execution"
            executeLines rest newSimbs
        _ -> return ()
executeLines _ _ = return ()

getFirstSmartLine :: String -> Bool -> (String, String)
getFirstSmartLine (a:xs) notFirst
    | a == '\n' && (not (null xs) && head xs /= ' ') && not notFirst = getFirstSmartLine xs True
    | notFirst = let (first, other) = getFirstSmartLine xs True
       in (first, a:other)
    | not notFirst = let (first, other) = getFirstSmartLine xs False
       in (a:first, other)
getFirstSmartLine _ _ = ("", "")

smartLines :: String -> [String]
smartLines [] = []
smartLines str = let (first, rest) = getFirstSmartLine str False
    in first : smartLines rest

exe :: [Ast] -> IO ()
exe mod = do
    ast <- codegen initModule mod
    putStrLn "./LLVM.IR Builded"

executeFile :: String -> IO()
executeFile fileName = do
    content <- readFile fileName
    res <- try $ evaluate $ exe (stringToAst content) :: IO (Either SomeException (IO()))
    case res of
        Left err -> if (isPrefixOf "Empty input." (show err)) then return () else putStrLn $ "\x1b[31mError:\x1b[0m " ++ show err
        Right ast -> exe (stringToAst content)