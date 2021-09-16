#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Text.Pandoc.JSON
import System.IO
import System.Posix.Temp
import System.Process
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Exception (bracket)

test :: Block -> IO Block
test cb@(CodeBlock (id, classes, namevals) contents) = do
    case elem "nat-ded" classes of
        False -> return cb
        True -> do
            let cleanup (name, handle) = hClose handle >> removeFile name
            bracket (mkstemp "nat-ded.svg") cleanup $ \(name, handle) -> do
                (name, handle) <- mkstemp "nat-ded.svg"
                readProcess "natural-deduction" ["2", name] (T.unpack contents)
                svg <- T.hGetContents handle
                hClose handle >> removeFile name
                return . RawBlock (Format "html") $ "<span class=\"math display\">" <> svg <> "</span>"

test b = return b

main :: IO ()
main = toJSONFilter test
