module Main where

import qualified Data.Text.IO as T
import Interpreter
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \args -> do
    if length args /= 1
    then putStrLn "Usage ellipse (bytecode|tree)"
    else do
        let filename = "./example.ll"
        input <- T.readFile filename
        case head args of
            "bytecode" -> runByteCodeInterpreter input
            "tree"     -> runTreeInterpreter input
            _          -> putStrLn "Invalid choice"

