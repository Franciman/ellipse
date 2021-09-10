module Main where

import qualified Data.Text.IO as T
import System.Environment (getArgs)

import Interpreter

main :: IO ()
main = getArgs >>= \args -> do
    let filename = head args
    input <- T.readFile filename
    runInterpreter input

