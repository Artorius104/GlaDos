module Main (main) where

import Core(mainLoop, executeFile)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    if (length args == 1)
        then executeFile (args !! 0)
        else mainLoop