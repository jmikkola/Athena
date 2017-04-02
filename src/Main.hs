module Main where

import Control.Monad.Except
import Data.Map (Map)
import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(..) )
import System.IO (stderr, hPutStrLn, hFlush)
import System.Process ( rawSystem )

import Parser ( parseFile )
import ParserTest ( testMain )

import Backends.Go.Convert (convertFile)
import Backends.Go.Emit (emitFile)
import IR
import Type (Type, TypeRef)
import TypeCheck (runTypechecking)
import qualified Backends.Interpreter.Interpreter as Interpreter

type ExitCodeResult = ExceptT String IO ExitCode

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

build :: String -> Either String (Map TypeRef Type, [IR.Decl])
build content = do
  parsed <- parseFile content
  runTypechecking parsed

interpret :: String -> IO ()
interpret content =
  case build content of
   Left err ->
     exitError err
   Right (types, ir) -> do
     exitCode <- Interpreter.run types ir
     exitWith exitCode

main :: IO ()
main = do
  args <- getArgs
  case args of
   ["--test"] -> testMain
   [fileName] -> do
     content <- readFile fileName
     interpret content
   ["--build", fileName] -> do
     content <- readFile fileName
     compileResult <- runExceptT $ compile content
     case compileResult of
      Left err       -> exitError err
      Right exitCode -> exitWith exitCode
   _ -> do
     putStrLn "usage: athena <file.at>"
     exitWith $ ExitFailure 1
