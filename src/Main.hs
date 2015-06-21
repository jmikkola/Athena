import System.Console.Readline

import Control.Applicative ( (<*) )
import Data.Char ( isSpace )

import Text.Parsec ( parse, eof )

import Eval
import Parse

main :: IO ()
main = repl "expression> " tryParsing

trim :: String -> String
trim = let revTrim = reverse . dropWhile isSpace
       in revTrim . revTrim

tryParsing :: String -> IO ()
tryParsing line = case parse (expression <* eof) "user input" line of
  Right parsed -> handleParsed parsed
  Left err -> print err

handleParsed :: Expression -> IO ()
handleParsed = print . translate

repl :: String -> (String -> IO ()) -> IO ()
repl prompt fn = do
  maybeLine <- readline prompt
  case maybeLine of
    Nothing -> return ()
    Just "end" -> return ()
    Just "exit" -> return ()
    Just line ->
      do
        addHistory line
        fn (trim line)
        repl prompt fn
