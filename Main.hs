module Main where

import System.Environment
import System.IO
import System.Exit

import qualified Parser as P

main = do contents <- getContents
          let output = P.parse contents
          putStrLn output

