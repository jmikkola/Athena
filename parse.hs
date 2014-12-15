module Parse where

import Control.Applicative ((<$>), (<*>))

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Prim
import Text.Parsec.String
import Text.Parsec.Token

data SExpression = List [SExpression]
                 | Symbol String
                 | Expr String
                 deriving (Eq, Show)

whitespaceChs = " \t\r\n"

anyWhitespaceCh :: Parser Char
anyWhitespaceCh = oneOf whitespaceChs

anyWhitespace :: Parser String
anyWhitespace = many $ anyWhitespaceCh

inCharsWhite :: Char -> Char -> (Parser a) -> (Parser a)
inCharsWhite start end inner = do
    char start
    anyWhitespace
    result <- inner
    anyWhitespace
    char end
    return result

inParens :: (Parser a) -> (Parser a)
inParens inner = inCharsWhite '(' ')' inner

parseList :: Parser SExpression
parseList = do
  contents <- inParens sexpressions
  return (List contents)

parseSymbol :: Parser SExpression
parseSymbol = do
  char ':'
  text <- many1 $ noneOf (whitespaceChs ++ "()")
  return (Symbol text)

parseItem :: Parser SExpression
parseItem = do
  text <- many1 $ noneOf (whitespaceChs ++ "()")
  return (Expr text)

sexpression :: Parser SExpression
sexpression = parseList <|> parseSymbol <|> parseItem

sexpressions :: Parser [SExpression]
sexpressions = sepEndBy sexpression anyWhitespace

parseSexpressions :: String -> Either ParseError [SExpression]
parseSexpressions input = parse sexpressions "???" input
