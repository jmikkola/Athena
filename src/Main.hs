module Main where

import System.Environment ( getArgs )

import Parser ( parseFile )
import ParserTest ( testMain )

main :: IO ()
main = do
  testMain
  args <- getArgs
  content <- readFile (args !! 0)
  putStrLn $ show $ parseFile content
