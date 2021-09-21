module Main where

import qualified Data.Text.IO as T
import Interpreter

main :: IO ()
main = do
    let filename = "./example.ll"
    input <- T.readFile filename
    runInterpreter input

