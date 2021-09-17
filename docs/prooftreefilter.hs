#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Text.Pandoc.JSON
import System.Process
import qualified Data.Text as T

naturalDed :: Block -> IO Block
naturalDed cb@(CodeBlock (id, classes, namevals) contents) = do
    case elem "nat-ded" classes of
        False -> return cb
        True -> do
            latex <- readProcess "natural-scheme" [] (T.unpack contents)
            let latex' = "\\begin{prooftree}\n" <> latex <> "\n\\end{prooftree}"
            return $ Plain [Math InlineMath (T.pack latex')]

naturalDed b = return b

main :: IO ()
main = toJSONFilter naturalDed
