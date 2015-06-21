module Parse where

import Text.Parsec
import Text.Parsec.String (Parser)

data LiteralValue = LiteralFloat Float | LiteralInt Int
                  deriving (Show, Eq)

unwrapOr :: Maybe a -> a -> a
unwrapOr (Just a) _ = a
unwrapOr Nothing  b = b

maybeEmpty :: Maybe String -> String
maybeEmpty m = unwrapOr m ""

literal :: Parser LiteralValue
literal = choice [numericLiteral]

numericLiteral :: Parser LiteralValue
numericLiteral = do
  start <- digits
  choice [float start, integer start]

integer :: String -> Parser LiteralValue
integer ds = return $ LiteralInt (read ds)

float :: String -> Parser LiteralValue
float ds = do
  fp <- floatingPart
  return $ LiteralFloat (read (ds ++ fp))

digits :: Parser String
digits = do
  d <- digit
  ds <- many _digit
  return (d : ds)

_digit :: Parser Char
_digit = do
  _ <- optional underscore
  digit

floatingPart :: Parser String
floatingPart = do
  _ <- char '.'
  ds <- optionMaybe digits
  exPart <- optionMaybe exponentPart
  return ('.' : (unwrapOr ds "0") ++ (maybeEmpty exPart))

exponentPart :: Parser String
exponentPart = do
  _ <- char 'e'
  sign <- choice [string "+", string "-", string ""]
  ds <- many digit
  return ('e' : sign ++ ds)

whitespaceChs :: String
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
