module Main where

import qualified Data.Text.IO as T
import System.Environment (getArgs)
import Data.Time.Clock
import Control.DeepSeq

import qualified CoreSyntaxTree as C
import Interpreter
import qualified Env as E
import qualified Data.Text as T
import Parser
import SyntaxTree
import Criterion.Main
import ByteCode


runBenchmarks :: T.Text -> IO ()
runBenchmarks input = do
    case parseProgram input of
        Left err -> putStrLn err
        Right defs -> do
            -- Let us first check that there is no duplicate definition
            case checkDuplicates (map declName defs) of
                Just name -> putStrLn $ "Error duplicate definition of: " ++ show name
                Nothing -> do
                    -- there is no duplication, so we can convert our program
                    -- to a core representation
                    let coreDefs = compile defs
                    defaultMain
                       [ bench "TypeCheck" $ nfIO $ typeCheckProgram E.empty coreDefs
                       , bench "Eval"      $ nfIO $ evalProgram emptyStack E.empty coreDefs
                       ]


main :: IO ()
main = do
    let filename = "./example.ll"
    input <- T.readFile filename
    runInterpreter input

