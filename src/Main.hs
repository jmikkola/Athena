module Main where

import System.Environment ( getArgs )
import System.Process ( rawSystem )
import System.Exit ( exitWith ) -- ExitCode (..)

import Parser ( parseFile )
import ParserTest ( testMain )
import TypeCheck ( checkFile )
import Emit ( showFile )

import TypeCheck2 ( runFile )
import Backends.Go.Convert (convertFile)
import Backends.Go.Emit (emitFile)

main :: IO ()
main = do
  testMain
  args <- getArgs
  content <- readFile (args !! 0)
  case parseFile content of
   Left err   -> putStrLn $ "parse error: " ++ err
   Right file -> do
     case runFile file of
      Left err -> putStrLn $ "error typing file: " ++ err
      Right t  -> case convertFile t of
        Left err -> putStrLn $ "error converting file: " ++ err
        Right f -> case emitFile f of
          Left err -> putStrLn $ "error emitting file: " ++ err
          Right txt -> putStrLn txt
     case checkFile file of
      Left err -> putStrLn $ "type error: " ++ err
      Right _  -> do
        let output = showFile file
        writeFile "out.go" output
        code <- rawSystem "go" ["build", "-o", "a.out", "out.go"]
        exitWith code
