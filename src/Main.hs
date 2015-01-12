import System.Console.Readline

import Parse

main :: IO ()
main = repl "> " tryParsing

tryParsing :: String -> IO ()
tryParsing line = putStrLn $ show $ parseSexpressions line

repl :: String -> (String -> IO ()) -> IO ()
repl prompt fn = do
  putStr prompt
  line <- getLine
  if line == "end"
    then return ()
    else do
      fn line
      repl prompt fn

