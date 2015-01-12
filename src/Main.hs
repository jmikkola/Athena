import System.Console.Readline

import SExpression
import Parse
import Read

main :: IO ()
main = repl "> " tryParsing

tryParsing :: String -> IO ()
tryParsing line = case parseSexpressions line of
  Right parsed -> do
    mapM handleExpr parsed
    return ()
  Left err -> print err

handleExpr :: SExpression -> IO ()
handleExpr sexpr = case readFunction sexpr of
    Right fn -> print fn
    Left err -> case readExpression sexpr of
      Right expr -> print expr
      Left err2 -> putStrLn $ unlines [
        "Couldn't parse as either function or expression", err, err2]

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
