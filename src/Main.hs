import System.Console.Readline

import Control.Applicative ( (<*) )
import Data.Char ( isSpace )
import qualified Data.Map as Map

import Text.Parsec ( parse, eof )

import Eval
import Parse

main :: IO ()
main = repl "expression> " tryParsing Map.empty

trim :: String -> String
trim = let revTrim = reverse . dropWhile isSpace
       in revTrim . revTrim

tryParsing :: String -> EvalContext -> IO EvalContext
tryParsing line ctx = case parse (statement <* eof) "user input" line of
  Right parsed -> handleParsed parsed ctx
  Left err -> do
    print err
    return ctx

handleParsed :: Statement -> EvalContext -> IO EvalContext
handleParsed stmt ctx = case evalStatement ctx stmt of
  Left err -> do
    print err
    return ctx
  Right (ctx', result) -> do
    print result
    return ctx'

repl :: String -> (String -> a -> IO a) -> a -> IO ()
repl prompt fn state = do
  maybeLine <- readline prompt
  case maybeLine of
    Nothing -> return ()
    Just "end" -> return ()
    Just "exit" -> return ()
    Just line ->
      do
        addHistory line
        state' <- fn (trim line) state
        repl prompt fn state'
