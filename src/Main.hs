module Main where

import System.Environment ( getArgs )

import Parser ( parseFile )
import ParserTest ( testMain )
import TypeCheck ( checkFile )

main :: IO ()
main = do
  --testMain
  args <- getArgs
  content <- readFile (args !! 0)
  case parseFile content of
   Left err   -> putStrLn $ "error: " ++ err
   Right file -> do
     --putStrLn $ show $ file
     putStrLn $ show $ checkFile file
