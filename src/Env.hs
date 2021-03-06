{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Env where
    
import qualified Data.Sequence as S

import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Control.DeepSeq

-- This module defines an environment useful for various
-- operations like typechecking or evaluation.
-- It associates various info to each variable.

-- Since we use index-based representation of variables,
-- the environment too uses index based accessing.

newtype Env a = Env
    { unwrap :: [a]
    }
    deriving(Show, NFData)

empty :: Env a
empty = Env []

-- We only support adding a new binding for the 0 variable (and shifting all the other indices),
-- this makes the implementation easier to be correct.
bind :: a -> Env a -> Env a
bind info (Env s) = Env $ info : s

-- Bind many variables, from left to right
{-# INLINE bindMany #-}
bindMany :: Foldable t => t a -> Env a -> Env a
bindMany xs e = foldl' (flip bind) e xs

-- Lookup info associated to the given variable, if any
{-# INLINE lookup #-}
lookup :: Int -> Env a -> Maybe a
lookup idx (Env s)
    | 0 <= idx && idx < length s = Just $ s !! idx
    | otherwise                  = Nothing

-- Unsafe lookup
{-# INLINE index #-}
index :: Int -> Env a -> a
index idx (Env s) = s !! idx
