module Main where

import Control.Monad.Except
import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(..) )
import System.IO (stderr, hPutStrLn, hFlush)

import Parser ( parseFile )
import ParserTest ( testMain )

type ExitCodeResult = ExceptT String IO ExitCode


main :: IO ()
main = do
  args <- getArgs
  case args of
   ["--test"] -> do
     testMain

   [fileName] -> do
     content <- readFile fileName
     interpret content

   _ -> do
     exitError "usage: athena <file.at>"

interpret :: String -> IO ()
interpret content =
  case parseFile content of
   Left err ->
     exitError err
   Right ast -> do
     putStrLn $ show ast
     exitWith ExitSuccess


either2Except :: Monad m => Either e a -> ExceptT e m a
either2Except = ExceptT . return

printErrLn :: String -> IO ()
printErrLn s = do
  hPutStrLn stderr s
  hFlush stderr

exitError :: String -> IO ()
exitError err = do
  printErrLn err
  exitWith (ExitFailure 1)
