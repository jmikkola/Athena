module Main where

import Control.Monad (liftM)
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

compile :: String -> IO (Either String Int)
compile content = do
  parsed <- (liftM parseFile) content
  checked <- (liftM runTypechecking) parsed
  goSyntax <- (liftM convertFile) checked
  output <- (liftM emitFile) goSyntax
  writeFile "out.go" output
  rawSystem "go" ["build", "-o", "a.out", "out.go"]

main :: IO ()
main = do
  testMain
  args <- getArgs
  content <- readFile (args !! 0)
  compileResult <- compile content
  case compileResult of
   Left err ->
     putStrLn $ err
   Right exitCode ->
     exitWith exitCode
