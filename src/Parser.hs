module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import AST.Declaration (File)
import AST.Expression (Expression, Op, Value)
import qualified AST.Expression as Expression
import AST.Statement (Statement)
import qualified AST.Statement as Statement
import AST.Type (Type)
import qualified AST.Type as Type

parse :: String -> File
parse _ = []


---- AST.Statement parsers ----

statementParser :: Parser Statement
statementParser = choice [
  returnStatement, letStatement, assignStatement,
  blockStatement, exprStatement, ifStatement, whileStatement]

returnStatement :: Parser Statement
returnStatement = do
  _ <- string "return"
  e <- optionMaybe $ do
    _ <- any1Whitespace
    expressionParser
  return $ Statement.Return e

letStatement :: Parser Statement
letStatement = do
  _ <- string "let"
  _ <- any1Whitespace
  name <- valueName
  _ <- any1Whitespace
  typ <- typeParser
  _ <- any1Whitespace
  _ <- char '='
  _ <- any1Whitespace
  val <- expressionParser
  return $ Statement.Let name typ val

assignStatement :: Parser Statement
assignStatement = do
  name <- valueName
  _ <- any1Whitespace
  _ <- char '='
  _ <- any1Whitespace
  val <- expressionParser
  return $ Statement.Assign name val

blockStatement :: Parser Statement
blockStatement = do
  _ <- char '{'
  stmts <- sepBy statementParser statementSep
  _ <- char '}'
  return $ Statement.Block stmts

statementSep :: Parser ()
statementSep = do
  _ <- anyWhitespace
  _ <- char '\n'
  _ <- anyWhitespace
  return ()

exprStatement :: Parser Statement
exprStatement = do
  e <- expressionParser
  return $ Statement.Expr e

ifStatement :: Parser Statement
ifStatement = do
  _ <- string "if"
  _ <- any1Whitespace
  test <- expressionParser
  _ <- any1Whitespace
  body <- blockStatement
  elsePart <- optionMaybe $ try elseBlock
  return $ let (Statement.Block stmts) = body
           in Statement.If test stmts elsePart

elseBlock :: Parser Statement
elseBlock = do
  _ <- any1Whitespace
  _ <- string "else"
  _ <- any1Whitespace
  ifStatement <|> blockStatement

whileStatement :: Parser Statement
whileStatement = do
  _ <- string "while"
  _ <- any1Whitespace
  test <- expressionParser
  _ <- any1Whitespace
  body <- blockStatement
  return $ let (Statement.Block stmts) = body
           in Statement.While test stmts

---- AST.Expression parsers ----

--- parse expressions

-- Binary expressions are handled at this level
expressionParser :: Parser Expression
expressionParser = do
  bin <- readBinExprParts
  return $ unfoldParts bin

readBinExprParts :: Parser (Expression, [(Op, Expression)])
readBinExprParts = do
  e <- expr
  _ <- anyWhitespace
  parts <- many $ do
    op <- opParser
    _ <- anyWhitespace
    e' <- expr
    return (op, e')
  return (e, parts)

unfoldParts :: (Expression, [(Op, Expression)]) -> Expression
unfoldParts bin =
  let (e, rest) = foldl unfoldOps bin precOrder
  in if rest /= [] then error "Unexpected operator"
     else e

unfoldOps :: (Expression, [(Op, Expression)]) -> [Op] -> (Expression, [(Op, Expression)])
unfoldOps (left, parts) ops = case parts of
  []              -> (left, [])
  ((op, right):pts) ->
    let (applied, rest) = unfoldOps (right, pts) ops
    in if elem op ops
       then (Expression.EBinary op left applied, rest)
       else (left, (op, applied) : rest)

precOrder :: [[Op]]
precOrder =
  [ [Expression.Times, Expression.Divide, Expression.Mod]
  , [Expression.Plus, Expression.Minus]
  , [Expression.Less, Expression.LessEq, Expression.Greater, Expression.GreaterEq]
  , [Expression.Eq, Expression.NotEq]
  , [Expression.BitAnd]
  , [Expression.BitXor]
  , [Expression.BitOr]
  , [Expression.BoolAnd]
  , [Expression.BoolOr]
  ]

expr :: Parser Expression
expr = choice [parenExpr, valueExpr, unaryExpr, callExpr, castExpr, varExpr]

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

unaryExpr :: Parser Expression
unaryExpr = do
  op <- opParser
  _ <- anyWhitespace
  ex <- expressionParser
  return $ Expression.EUnary op ex

callExpr :: Parser Expression
callExpr = do
  fn <- varExpr
  _ <- char '('
  _ <- anyWhitespace
  args <- choice [fnCallArg, argsEnd]
  return $ Expression.ECall fn args

fnCallArg :: Parser [Expression]
fnCallArg = do
  arg <- expressionParser
  _ <- anyWhitespace
  rest <- choice [fnCallNextArg, argsEnd]
  return $ arg : rest

fnCallNextArg :: Parser [Expression]
fnCallNextArg = do
  _ <- char ','
  _ <- anyWhitespace
  fnCallArg

argsEnd :: Parser [Expression]
argsEnd = do
  _ <- char '('
  return []

castExpr :: Parser Expression
castExpr = do
  typ <- typeParser
  _ <- char '('
  _ <- anyWhitespace
  ex <- expressionParser
  _ <- anyWhitespace
  _ <- char ')'
  return $ Expression.ECast typ ex

varExpr :: Parser Expression
varExpr = do
  name <- valueName
  return $ Expression.EVariable name

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
opParser = choices opChoices

opChoices :: [(String, Op)]
opChoices =
      [ ("+",  Expression.Plus)
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

choices :: [(String, a)] -> Parser a
choices = choice . map pair2parser

pair2parser :: (String, a) -> Parser a
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

anyWhitespace :: Parser String
anyWhitespace = do
  whitespaces <- many $ anyWhitespaceS
  return $ concat whitespaces

any1Whitespace :: Parser String
any1Whitespace = do
  whitespaces <- many1 $ anyWhitespaceS
  return $ concat whitespaces

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
