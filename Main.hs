module Main where

import System.Environment
import System.IO
import System.Exit

import qualified Lexer as L

main = do contents <- getContents
          let output = L.dbgParseAll contents
          putStrLn output

