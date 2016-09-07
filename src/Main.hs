module Main where

import Control.Monad (liftM)
import Control.Monad.Error
import System.Environment ( getArgs )
import System.Process ( rawSystem )
import System.Exit ( exitWith ) -- ExitCode (..)

import Parser ( parseFile )
import ParserTest ( testMain )
import TypeCheck ( checkFile )
import Emit ( showFile )

import TypeCheck2 (runTypechecking)
import Backends.Go.Convert (convertFile)
import Backends.Go.Emit (emitFile)

--compile :: String -> ErrorT String IO Int
compile content = do
  parsed <- parseFile content
  checked <- runTypechecking parsed
  goSyntax <- convertFile checked
  output <- emitFile goSyntax
  liftIO $ writeFile "out.go" output
  liftIO $ rawSystem "go" ["build", "-o", "a.out", "out.go"]

main :: IO ()
main = do
  testMain
  args <- getArgs
  content <- readFile (args !! 0)
  compileResult <- runErrorT compile content
  case compileResult of
   Left err ->
     putStrLn $ err
   Right exitCode ->
     exitWith exitCode
