module Main where

import Control.Applicative ( (<*) )
import Data.Char ( isSpace )
import System.Environment ( getArgs )

import System.Console.Haskeline ( InputT, getInputLine, runInputT, defaultSettings, outputStrLn )
import Text.Parsec ( parse, eof )

import Eval
import Parse hiding ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ show args
  runInputT defaultSettings (loop emptyContext)

loop :: EvalContext -> InputT IO ()
loop ctx = do
  minput <- getInputLine "athena> "
  case minput of
   Nothing     -> return ()
   Just "end"  -> return ()
   Just "exit" -> return ()
   Just line   -> do
      ctx' <- tryParsing (trim line) ctx
      loop ctx'

trim :: String -> String
trim = let revTrim = reverse . dropWhile isSpace
       in revTrim . revTrim

tryParsing :: String -> EvalContext -> InputT IO EvalContext
tryParsing line ctx = case parse (statement <* eof) "user input" line of
  Right parsed -> handleParsed parsed ctx
  Left err -> do
    outputStrLn $ show err
    return ctx

handleParsed :: Statement -> EvalContext -> InputT IO EvalContext
handleParsed stmt ctx = case evalStatement ctx stmt of
  Left err -> do
    outputStrLn err
    return ctx
  Right (ctx', result) -> do
    outputStrLn $ show result
    return ctx'
