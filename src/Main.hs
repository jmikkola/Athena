import System.Console.Readline

import Parse

main :: IO ()
main = repl "> " tryParsing

tryParsing :: String -> IO ()
tryParsing line = putStrLn $ show $ parseSexpressions line

repl :: String -> (String -> IO ()) -> IO ()
repl prompt fn = do
  maybeLine <- readline prompt
  case maybeLine of
    Nothing -> return ()
    Just "end" -> return ()
    Just "exit" -> return ()
    Just line -> do
                 addHistory line
                 fn line
                 repl prompt fn
