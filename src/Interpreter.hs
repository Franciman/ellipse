{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Interpreter where
    
-- In this module we tie together all the ties to fully interpret a program:
-- from the parsing to the evaluation.

import Parser
import SyntaxTree
import Eval
import Type
import TypeCheck
import Control.Monad (forM)
import qualified CoreSyntaxTree as Core

import qualified Data.Text as T
import qualified Data.Set as S
import qualified Env as E

-- Find if there is any duplicate, and report the first one found
-- it runs in O(n*logn) worst case time.
checkDuplicates :: [T.Text] -> Maybe T.Text
checkDuplicates names = go S.empty names
    where go :: S.Set T.Text -> [T.Text] -> Maybe T.Text
          go _ [] = Nothing
          go s (n:ns)
            | S.member n s = Just n
            | otherwise    = go (S.insert n s) ns

-- When performing typechecking we need to take into account that definitions
-- can only depend on previous definitions, so each definition that gets
-- typechecked is added to the environment for the next ones
typeCheckProgram :: TypingEnv -> [Core.Decl] -> IO (Maybe String)
typeCheckProgram _ [] = return Nothing
typeCheckProgram env (Core.Decl name body:ds) = do
    let tp = typeCheck env E.empty body
    case tp of
        Left err -> return (Just err)
        Right ty -> do
            putStrLn $ show name ++ " has type: " ++ show ty
            typeCheckProgram (E.bind ty env) ds


-- Now let us evaluate all definitions, until we get to the main
-- program, it will give us our value. Observe that
-- given the fact that each definition only depends from the
-- previous ones, it is guaranteed that each definition can be
-- full computed, if we proceed in order.
-- Each time a definition is computed, it is added to the evaluator's envtype Env = E.Env Value
evalProgram :: Env -> [Core.Decl] -> IO Value
evalProgram _ [] = return $ BoolLit False -- If there is no main, return a dummy value
evalProgram env (Core.Decl name body:ds) = do
    let val = runEval env body
    if name == "main"
    then return val
    else evalProgram (E.bind val env) ds


runInterpreter :: T.Text -> IO ()
runInterpreter input = do
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
                    error <- typeCheckProgram E.empty coreDefs
                    case error of
                        Just err -> putStrLn $ "TypeCheck error: " ++ err
                        Nothing -> do
                            res <- evalProgram E.empty coreDefs
                            print res
