module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import AST.Declaration (Declaraction, File)
import AST.Expression (Expression, Op, Value)
import qualified AST.Expression as Expression
import AST.Type (Type)
import qualified AST.Type as Type

parse :: String -> File
parse _ = []

---- AST.Expression parsers ----

--- parse expressions

expressionParser :: Parser Expression
expressionParser = choice [parenExpr, valueExpr]

parenExpr :: Parser Expression
parenExpr = do
  _ <- char '('
  _ <- anyWhitespaceS
  ex <- expressionParser
  _ <- anyWhitespaceS
  _ <- char ')'
  return $ Expression.EParen ex

valueExpr :: Parser Expression
valueExpr = do
  val <- valueParser
  return $ Expression.EValue val

--- parse values

valueParser :: Parser Value
valueParser = choice [stringParser, boolParser, numberParser]

stringParser :: Parser Value
stringParser = do
  s <- doubleQuotedString
  return $ Expression.EString s

boolParser :: Parser Value
boolParser = do
  b <- choices [("False", False), ("True", True)]
  return $ Expression.EBool b

--- parse floating and integer numbers

numberParser :: Parser Value
numberParser = do
  start <- digits
  choice [float start, integer start]

float :: String -> Parser Value
float start = do
  fp <- floatingPart
  return $ Expression.EFloat (read (start ++ fp))

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

digits :: Parser String
digits = do
  d <- digit
  ds <- many _digit
  return (d : ds)

_digit :: Parser Char
_digit = do
  _ <- optional underscore
  digit

integer :: String -> Parser Value
integer start = return $ Expression.EInt (read start)

--- parse operators

opParser :: Parser Op
opParser = choices ops

ops :: [(String, Op)]
ops = [ ("+",  Expression.Plus)
      , ("-",  Expression.Minus)
      , ("*",  Expression.Times)
      , ("/",  Expression.Divide)
      , ("%",  Expression.Mod)
      , ("**", Expression.Power)
      , ("&",  Expression.BitAnd)
      , ("|",  Expression.BitOr)
      , ("~",  Expression.BitInvert)
      , ("^",  Expression.BitXor)
      , ("&&", Expression.BoolAnd)
      , ("||", Expression.BoolOr)
      , ("!",  Expression.BoolNot)
      , ("==", Expression.Eq)
      , ("!=", Expression.NotEq)
      , ("<",  Expression.Less)
      , ("<=", Expression.LessEq)
      , (">",  Expression.Greater)
      , (">=", Expression.GreaterEq)
      ]

---- AST.Type parsers ----

typeParser :: Parser Type
typeParser = choices types

types :: [(String, Type)]
types = [ ("String", Type.String)
        , ("Float", Type.Float)
        , ("Int", Type.Int)
        , ("Bool", Type.Bool)
        , ("()", Type.Nil)
        ]

---- Helper functions ----

choices = choice . map pair2parser

pair2parser (str, result) = do
  _ <- string str
  return result

doubleQuotedString :: Parser String
doubleQuotedString = do
  _ <- char '"'
  stringContents <- many $ choice [escapedChar, many1 $ noneOf "\\\""]
  _ <- char '"'
  return $ concat stringContents

escapedChar :: Parser String
escapedChar = do
  _ <- char '\\'
  c <- anyChar
  return $ '\\' : c : ""

typeName :: Parser String
typeName = do
  first <- upper
  rest <- many $ choice [letter, underscore]
  return $ first : rest

valueName :: Parser String
valueName = do
  first <- lower
  rest <- many $ choice [alphaNum, underscore, char '?']
  return $ first : rest

anyWhitespaceS :: Parser String
-- `try` is needed here so that it can back out of parsing a division operator
anyWhitespaceS = many1 anyWhitespaceCh <|> try parseComment

whitespaceChs :: String
whitespaceChs = " \t\r\n"

anyWhitespaceCh :: Parser Char
anyWhitespaceCh = oneOf whitespaceChs

parseComment :: Parser String
parseComment = try parseLineComment <|> parseBlockComment <?> "Comment"

parseLineComment :: Parser String
parseLineComment = do
  start <- string "//"
  comment <- many $ noneOf "\r\n"
  return $ start ++ comment

parseBlockComment :: Parser String
parseBlockComment = do
  start <- string "/*"
  contentList <- blockCommentContents
  return $ concat $ start : contentList

blockCommentContents :: Parser [String]
blockCommentContents = starContent <|> nonStarConents

nonStarConents :: Parser [String]
nonStarConents = do
  content <- many1 $ noneOf "*"
  rest <- starContent
  return $ content : rest

starContent :: Parser [String]
starContent = try blockCommentEnd <|> starRest

starRest :: Parser [String]
starRest = do
  star <- string "*"
  rest <- blockCommentContents
  return $ star : rest

blockCommentEnd :: Parser [String]
blockCommentEnd = do
  end <- string "*/"
  return [end]

underscore :: Parser Char
underscore = char '_'

unwrapOr :: Maybe a -> a -> a
unwrapOr (Just a) _ = a
unwrapOr Nothing  b = b

maybeEmpty :: Maybe String -> String
maybeEmpty m = unwrapOr m ""
