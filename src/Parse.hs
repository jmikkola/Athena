module Parse where

import Control.Applicative ((<$>), (<*>))

import Text.Parsec
import Text.Parsec.Char (char, digit, satisfy)
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Prim
import Text.Parsec.String
import Text.Parsec.Token

data LiteralValue = LiteralFloat Float | LiteralInt Int
                  deriving (Show, Eq)

digits :: Parser String
digits = do
  d <- digit
  ds <- many _digit
  return (d : ds)

_digit :: Parser Char
_digit = do
  _ <- optional underscore
  digit

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
