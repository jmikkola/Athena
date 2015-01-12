import Parse

main :: IO ()
main = do
  putStr "> "
  inp <- getLine
  putStrLn $ show $ parseSexpressions inp
