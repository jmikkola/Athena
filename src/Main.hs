module Main where

import Control.Applicative ( (<*) )
import Control.Monad ( foldM )
import Data.Char ( isSpace )
import System.Environment ( getArgs )

import System.Console.Haskeline ( InputT, getInputLine, runInputT, defaultSettings, outputStrLn )
import Text.Parsec ( parse, eof )

import Eval
import Parse hiding ( getArgs )
import TypeInference
import InferExpression

findMatchingFlags :: [String] -> String -> [String]
findMatchingFlags (f:value:fs) flag = if f == flag then value : rest else rest
  where rest = findMatchingFlags (value:fs) flag
findMatchingFlags _            _    = []

main :: IO ()
main = do
  args <- getArgs
  ctx <- foldM runFile emptyContext (findMatchingFlags args "-f")
  runInputT defaultSettings (loop ctx)

runFile :: EvalContext -> String -> IO EvalContext
runFile ctx filename = do
  contents <- readFile filename
  case evalFile ctx filename contents of
   Right ctx' -> return ctx'
   Left  err  -> do
     putStrLn ("Error evaluating " ++ filename ++ " " ++ err)
     return ctx

evalFile :: EvalContext -> String -> String -> Either String EvalContext
evalFile ctx filename contents = do
  statements <- applyLeft show $ parse parseFile filename contents
  foldM handleStatement ctx statements

applyLeft :: (a -> b) -> (Either a r) -> (Either b r)
applyLeft fn (Left  a) = Left (fn a)
applyLeft _  (Right r) = Right r

handleStatement :: EvalContext -> Statement -> Either String EvalContext
handleStatement ctx stmt = do
  (ctx', _) <- evalStatement ctx stmt
  return ctx'

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
    outputStrLn $ display result
    return ctx'
