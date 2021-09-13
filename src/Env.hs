{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Env where
    
import qualified Data.Sequence as S

import Data.Foldable (foldl')

import Control.DeepSeq

-- This module defines an environment useful for various
-- operations like typechecking or evaluation.
-- It associates various info to each variable.

-- Since we use index-based representation of variables,
-- the environment too uses index based accessing.

newtype Env a = Env
    { unwrap :: S.Seq a
    }
    deriving(Show, NFData)

empty :: Env a
empty = Env S.empty

-- We only support adding a new binding for the 0 variable (and shifting all the other indices),
-- this makes the implementation easier to be correct.
bind :: a -> Env a -> Env a
bind info (Env s) = Env $ info S.<| s

-- Bind many variables, from left to right
bindMany :: Foldable t => t a -> Env a -> Env a
bindMany xs e = foldl' (flip bind) e xs

-- Lookup info associated to the given variable, if any
lookup :: Int -> Env a -> Maybe a
lookup idx (Env s) = S.lookup idx s
