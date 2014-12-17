module Parse where

import Control.Applicative ((<$>), (<*>))

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Prim
import Text.Parsec.String
import Text.Parsec.Token

data SExpression = List [SExpression]
                 | CharValue Char
                 | StringValue String
                 | Key String
                 | Symbol String
                 | IntVal Int
                 | FloatVal Float
                 deriving (Eq, Show)

whitespaceChs = " \t\r\n"

otherChars :: Parser Char
otherChars = oneOf "~!@$%^&*-+=<>?"

underscore :: Parser Char
underscore = char '_'

anyKeyChar :: Parser Char
anyKeyChar = alphaNum <|> underscore <|> otherChars

symbolStartChar :: Parser Char
symbolStartChar = letter <|> underscore <|> otherChars

anyWhitespaceCh :: Parser Char
anyWhitespaceCh = oneOf whitespaceChs

anyWhitespace :: Parser String
anyWhitespace = many $ anyWhitespaceCh

listStart = char '('

listEnd :: [SExpression] -> Parser SExpression
listEnd soFar = do
  char ')'
  return $ List $ reverse soFar

parseListNext :: [SExpression] -> Parser SExpression
parseListNext soFar = do
  nextItem <- sexpression
  parseListContents (nextItem : soFar)

parseListContents :: [SExpression] -> Parser SExpression
parseListContents soFar = (listEnd soFar) <|>
                          (do
                              many anyWhitespaceCh
                              listEnd soFar <|> parseListNext soFar)

parseList :: Parser SExpression
parseList = do
  listStart
  parseListContents []

parseChar :: Parser SExpression
parseChar = do
    char '\''
    c <- anyChar
    char '\''
    return $ CharValue c

stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

stringChar = do
  c <- stringLetter
  return (Just c)

parseString :: Parser SExpression
parseString = do
  s <- between
       (char '"')
       (char '"' <?> "end of string")
       (many stringChar)
  return $ StringValue (foldr (maybe id (:)) "" s)

parseKey :: Parser SExpression
parseKey = do
  char ':'
  text <- many1 $ anyKeyChar
  return (Key text)

parseInt :: String -> Parser SExpression
parseInt digits = do
  return (IntVal $ (read digits))

parseFloat :: String -> Parser SExpression
parseFloat digits = do
  char '.'
  decimal <- many1 $ digit
  return (FloatVal $ read $ (digits ++ "." ++ decimal))

parseNumber :: Parser SExpression
parseNumber = do
  digits <- many1 $ digit
  parseFloat digits <|> parseInt digits

parseSymbol :: Parser SExpression
parseSymbol = do
  c1 <- symbolStartChar
  text <- many $ anyKeyChar
  return $ Symbol $ c1 : text

sexpression :: Parser SExpression
sexpression = parseList <|>
              parseString <|>
              parseChar <|>
              parseKey <|>
              parseNumber <|>
              parseSymbol

sexpressions :: Parser [SExpression]
sexpressions = do
  expressions <- sepEndBy1 sexpression anyWhitespace
  eof
  return expressions

parseSexpressions :: String -> Either ParseError [SExpression]
parseSexpressions input = parse sexpressions "???" input
