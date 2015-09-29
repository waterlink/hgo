module Main where

import AutoSemi

import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

repl = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "ready> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> (liftIO $ process input) >> loop

process :: String -> IO (Maybe String)
process source = do
  let res = addAutoSemi source
  case res of
    Left err -> print err >> return Nothing
    Right output -> do
      putStrLn output
      return $ Just output

processFile :: String -> IO (Maybe String)
processFile fname = readFile fname >>= process

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> processFile fname >> return ()
