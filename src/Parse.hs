module Parse where

import Control.Monad ( liftM )
import Data.Char ( digitToInt )

import Text.Parsec
import Text.Parsec.String (Parser)

type Block = [Statement]

-- TODO: add function declaration, type declaration
data Statement = StatementExpr Expression
               | StatementAssign VariableName Expression
               | StatementReturn Expression
               | StatementIf { condition :: Expression
                             , body :: Block
                             , elseIfBlocks :: [(Expression, Block)]
                             , elseBlock :: Maybe Block
                             }
               | StatementWhile Expression Block
               | StatementFor VariableName Expression Block
               deriving (Show, Eq)

data BinaryOp = Plus | Minus | Times | Divide | Mod | Power
              deriving (Show, Eq)

data UnaryOp = Negate | Flip
             deriving (Show, Eq)

data Expression = ExpressionLit LiteralValue
                | ExpressionVar VariableName
                | ExpressionParen Expression
                | ExpressionFnCall FunctionName [Expression]
                | ExpressionBinary BinaryOp Expression Expression
                | ExpressionUnary UnaryOp Expression
                deriving (Show, Eq)

-- TODO: allow using structs with fields...
data LiteralValue = LiteralFloat Float | LiteralInt Int | LiteralString String | LiteralStruct String
                  deriving (Show, Eq)

type VariableName = String
type FunctionName = String

unwrapOr :: Maybe a -> a -> a
unwrapOr (Just a) _ = a
unwrapOr Nothing  b = b

maybeEmpty :: Maybe String -> String
maybeEmpty m = unwrapOr m ""

-- Both assignmentStatement and returnStatement are safe to "try" because they will fail fast
statement :: Parser Statement
statement = choice [ try assignmentStatement
                   , try returnStatement
                   , expressionStatment
                   ]

expressionStatment :: Parser Statement
expressionStatment = liftM StatementExpr $ expression

assignmentStatement :: Parser Statement
assignmentStatement = do
  _ <- letKwd
  _ <- anyWhitespace
  var <- valueName
  _ <- anyWhitespace
  _ <- char '='
  _ <- anyWhitespace
  expr <- expression
  return $ StatementAssign var expr

returnStatement :: Parser Statement
returnStatement = do
  _ <- returnKwd
  _ <- anyWhitespace
  expr <- expression
  return $ StatementReturn expr

expression :: Parser Expression
expression = binLevel1

nonBinaryExpression ::  Parser Expression
nonBinaryExpression = choice [ parenExpression
                             , literalExpression
                             , lowerLetterExpr
                             ]

-- Expressions that start with a lowercase letter
lowerLetterExpr :: Parser Expression
lowerLetterExpr = do
  name <- valueName
  choice [functionCallExpression name, variableExpression name]

literalExpression :: Parser Expression
literalExpression = liftM ExpressionLit $ literal

literal :: Parser LiteralValue
literal = choice [try hexLiteral, try octalLiteral, numericLiteral, stringLiteral, structLiteral]

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

hexLiteral :: Parser LiteralValue
hexLiteral = liftM LiteralInt $ hexNum

readInBase :: Int -> String -> Int
readInBase base = foldl (\current chr -> (digitToInt chr) + (base * current)) 0

hexNum :: Parser Int
hexNum = do
  _ <- string "0x"
  hexits <- many1 $ oneOf "0123456789ABCDEFabcdef"
  return $ (readInBase 16) hexits

octalLiteral :: Parser LiteralValue
octalLiteral = liftM LiteralInt $ octalNum

octalNum :: Parser Int
octalNum = do
  _ <- string "0o"
  octits <- many1 $ oneOf "01234567"
  return $ (readInBase 8) octits

stringLiteral :: Parser LiteralValue
stringLiteral = liftM LiteralString $ doubleQuotedString

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

-- TODO: parse field values
structLiteral :: Parser LiteralValue
structLiteral = liftM LiteralStruct $ typeName

variableExpression :: String -> Parser Expression
variableExpression varName = return $ ExpressionVar varName

parenExpression :: Parser Expression
parenExpression = do
  _ <- char '('
  _ <- anyWhitespace
  expr <- expression
  _ <- anyWhitespace
  _ <- char ')'
  return $ ExpressionParen expr

functionCallExpression :: String -> Parser Expression
functionCallExpression fnName = do
  _ <- anyWhitespace
  args <- fnCallArgs
  return $ ExpressionFnCall fnName args

fnCallArgs :: Parser [Expression]
fnCallArgs = do
  _ <- char '('
  _ <- anyWhitespace
  fnCallArgsList

fnCallArgsList :: Parser [Expression]
fnCallArgsList = choice [fnCallArgsEnd, fnCallArgsSingle]

fnCallArgsSingle :: Parser [Expression]
fnCallArgsSingle = do
  expr <- expression
  _ <- anyWhitespace
  rest <- choice [fnCallArgsNext, fnCallArgsEnd]
  return (expr : rest)

fnCallArgsNext :: Parser [Expression]
fnCallArgsNext = do
  _ <- char ','
  _ <- anyWhitespace
  fnCallArgsSingle

fnCallArgsEnd :: Parser [Expression]
fnCallArgsEnd = do
  _ <- char ')'
  return []

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

commaSeparator :: Parser ()
commaSeparator = do
  _ <- anyWhitespace
  _ <- char ','
  _ <- anyWhitespace
  return ()

_binaryOp :: String -> BinaryOp -> Parser BinaryOp
_binaryOp s op = do
  _ <- string s
  return op

plusOp = _binaryOp "+" Plus
minusOp = _binaryOp "-" Minus
timesOp = _binaryOp "*" Times
divideOp = _binaryOp "/" Divide
modOp = _binaryOp "%" Mod

binaryOp = choice [plusOp, minusOp, timesOp, divideOp, modOp]

binaryOps1 = choice [plusOp, minusOp]
binaryOps2 = choice [timesOp, divideOp, modOp]

binLevel1 :: Parser Expression
binLevel1 = choice [try binExpr1, binLevel2]

binLevel2 :: Parser Expression
binLevel2 = choice [try binExpr2, nonBinaryExpression]

binExpr1 :: Parser Expression
binExpr1 = do
  left <- binLevel2
  _ <- anyWhitespace
  op <- binaryOps1
  _ <- anyWhitespace
  right <- binLevel1
  return $ ExpressionBinary op left right

binExpr2 :: Parser Expression
binExpr2 = do
  left <- nonBinaryExpression
  _ <- anyWhitespace
  op <- binaryOps2
  _ <- anyWhitespace
  right <- binLevel2
  return $ ExpressionBinary op left right

ifKwd :: Parser String
ifKwd = string "if"

elseKwd :: Parser String
elseKwd = string "else"

whileKwd :: Parser String
whileKwd = string "while"

forKwd :: Parser String
forKwd = string "for"

inKwd :: Parser String
inKwd = string "in"

letKwd :: Parser String
letKwd = string "let"

returnKwd :: Parser String
returnKwd = string "return"

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
