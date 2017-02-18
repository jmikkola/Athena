module Main where

import Control.Monad.Except
import System.Environment ( getArgs )
import System.Process ( rawSystem )
import System.Exit ( exitWith, ExitCode(..) )

import Parser ( parseFile )
import ParserTest ( testMain )

import TypeCheck (runTypechecking)
import Backends.Go.Convert (convertFile)
import Backends.Go.Emit (emitFile)

type ExitCodeResult = ExceptT String IO ExitCode

either2Except :: Monad m => Either e a -> ExceptT e m a
either2Except = ExceptT . return

compileAndEmit :: String -> Either String String
compileAndEmit content = do
  parsed <- parseFile content
  (types, checked) <- runTypechecking parsed
  goSyntax <- convertFile types checked
  emitFile goSyntax

compile :: String -> ExitCodeResult
compile content = do
  output <- either2Except $ compileAndEmit content
  _ <- lift $ writeFile "out.go" output
  lift $ rawSystem "go" ["build", "-o", "a.out", "out.go"]

main :: IO ()
main = do
  args <- getArgs
  case args of
   ["--test"] -> testMain
   [fileName] -> do
     content <- readFile fileName
     compileResult <- runExceptT $ compile content
     case compileResult of
      Left err       -> do
        putStrLn $ err
        exitWith $ ExitFailure 1
      Right exitCode -> exitWith exitCode
   _ -> do
     putStrLn "usage: athena <file.at>"
     exitWith $ ExitFailure 1
