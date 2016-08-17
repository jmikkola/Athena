module Main where

import System.Environment ( getArgs )

import Parser ( parse )

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  putStrLn $ parse content
