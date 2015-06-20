module Parse where

import Control.Applicative ((<$>), (<*>))

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Prim
import Text.Parsec.String
import Text.Parsec.Token

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

stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

stringChar = do
  c <- stringLetter
  return (Just c)
