module Core(
    executeFile,
    exe
) where


import Parser(Ast(..), Parser, stringToAst)
import Data.Char()
import Data.Maybe()
import Data.Map(empty)
import Op
import Gen
import Data.Typeable(typeOf)
import Control.Exception(try, SomeException, evaluate)
import Data.List(isPrefixOf)


--mainLoop :: IO ()
--mainLoop = loopAst empty

--loopAst :: AssignedSymbols -> IO()
--loopAst symbs = do
--    input <- getLine
--    if input == "quit" || input == "exit"
--        then return ()
--        else do
--            putStrLn "Parsed: "
--            let parse = Nothing
--            printMaybeAst parse "Error in parsing"
--            putStrLn "Executed: "
--            case parse of
--                Nothing -> loopAst symbs
--                Just ast -> do
--                    let (res, newSymbs) = evalAST (ast, symbs)
--                    printMaybeAst res "Error in execution"
--                    loopAst newSymbs
--
--executeLines :: [String] -> AssignedSymbols -> IO()
--executeLines (line:rest) symbs = do
--    let maybeAst = Nothing
--    case maybeAst of
--        Just ast -> do
--            let (res, newSimbs) = evalAST (ast, symbs)
--            printMaybeAst res "Error in execution"
--            executeLines rest newSimbs
--        _ -> return ()
--executeLines _ _ = return ()
--
--getFirstSmartLine :: String -> Bool -> (String, String)
--getFirstSmartLine (a:xs) notFirst
--    | a == '\n' && (not (null xs) && head xs /= ' ') && not notFirst = getFirstSmartLine xs True
--    | notFirst = let (first, other) = getFirstSmartLine xs True
--       in (first, a:other)
--    | not notFirst = let (first, other) = getFirstSmartLine xs False
--       in (a:first, other)
--getFirstSmartLine _ _ = ("", "")
--
--smartLines :: String -> [String]
--smartLines [] = []
--smartLines str = let (first, rest) = getFirstSmartLine str False
--    in first : smartLines rest


m :: String -> String
m [] = "LLVM"
m s = s

exe :: [Ast]-> String -> IO ()
exe ast name = do
    _ <- codegen initModule ast name
    putStrLn (m name  ++ ".IR Builded")

executeFile :: [String] -> String -> IO()
executeFile filepath name = do
    content <- mapM readFile filepath
    print content
    exe (stringToAst (unwords content)) name
