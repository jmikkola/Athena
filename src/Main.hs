module Main where

import System.Environment ( getArgs )
import System.Process ( rawSystem )
import System.Exit ( exitWith ) -- ExitCode (..)

import Parser ( parseFile )
import ParserTest ( testMain )
import TypeCheck ( checkFile )
import Emit ( showFile )

import TypeCheck2 ( Scope )

main :: IO ()
main = do
  testMain
  args <- getArgs
  content <- readFile (args !! 0)
  case parseFile content of
   Left err   -> putStrLn $ "parse error: " ++ err
   Right file -> do
     case checkFile file of
      Left err -> putStrLn $ "type error: " ++ err
      Right _  -> do
        let output = showFile file
        writeFile "out.go" output
        code <- rawSystem "go" ["build", "-o", "a.out", "out.go"]
        exitWith code
