module Parser.BinaryExpr where

import Text.Parsec ( char
                   , choice
                   , digit
                   , many1
                   , optionMaybe
                   , string )
import Text.Parsec.String ( Parser )

class Unparse a where
  unparse :: a -> String

data BinaryOp = Plus | Minus | Times | Divide | Mod | Power
              | Less | LessEq | Equals | Greater | GreaterEq | NotEq
              | And | Or
              deriving (Show, Eq)

instance Unparse BinaryOp where
  unparse Plus      = "+"
  unparse Minus     = "-"
  unparse Times     = "*"
  unparse Divide    = "/"
  unparse Mod       = "%"
  unparse Power     = "^"
  unparse Less      = "<"
  unparse LessEq    = "<="
  unparse Equals    = "=="
  unparse Greater   = ">"
  unparse GreaterEq = ">="
  unparse NotEq     = "!="
  unparse And       = "&&"
  unparse Or        = "||"

data Expr = ExprInt Int
          | ExprParen Expr
          | ExprBin Expr BinaryOp Expr
          deriving (Show, Eq)

instance Unparse Expr where
  unparse (ExprInt i) = show i
  unparse (ExprParen e) = "(" ++ unparse e ++ ")"
  unparse (ExprBin l op r) =  unparse l ++ unparse op ++ unparse r

parseNumber ::  Parser Int
parseNumber = do
  digits <- many1 $ digit
  return $ read digits

parseExprInt :: Parser Expr
parseExprInt = do
  i <- parseNumber
  return $ ExprInt i

parseExprParen :: Parser Expr
parseExprParen = do
  char '('
  e <- parseExpression
  char ')'
  return $ ExprParen e

parseFactor :: Parser Expr
parseFactor = choice [parseExprParen, parseExprInt]

parseExpression :: Parser Expr
parseExpression = makeBinaryParser binaryOpLevels

binaryOpLevels :: [[BinaryOp]]
binaryOpLevels = [ [LessEq, GreaterEq, NotEq, Equals, Less, Greater]
                 , [Plus, Minus, And, Or]
                 , [Times, Divide, Mod]
                 , [Power]
                 ]

makeOpParser :: BinaryOp -> Parser BinaryOp
makeOpParser op = do
  string (unparse op)
  return op

data PartialOp = PartialOp BinaryOp Expr
               | ContinuedOp BinaryOp Expr PartialOp
               deriving (Eq, Show)

makeBinaryParser :: [[BinaryOp]] -> Parser Expr
makeBinaryParser []           = parseFactor
makeBinaryParser (level:rest) =
  let nextParser = makeBinaryParser rest
      opParsers  = map makeOpParser level

      opLevel    = do
        operator  <- choice opParsers
        operand   <- nextParser
        continued <- optionMaybe opLevel
        return $ case continued of
          Nothing  -> PartialOp operator operand
          (Just e) -> ContinuedOp operator operand e

      exprLevel  = do
        operand   <- nextParser
        operation <- optionMaybe opLevel
        return $ case operation of
          Nothing        -> operand
          (Just partial) -> foldExpr operand partial
  in exprLevel

-- Undoes the effects of removing left recursion in the grammar
foldExpr :: Expr -> PartialOp -> Expr
foldExpr e partial =
  case partial of
   (PartialOp op expr) -> ExprBin e op expr
   (ContinuedOp op expr rest) -> foldExpr (ExprBin e op expr) rest
