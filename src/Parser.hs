module Parser where

import Control.Monad ( liftM )
import Text.Parsec
import Text.Parsec.String (Parser)

import AST.Declaration (Declaraction, File)
import qualified AST.Declaration as Declaraction
import AST.Expression (Expression, BinOp, UnaryOp, Value)
import qualified AST.Expression as Expression
import AST.Statement (Statement)
import qualified AST.Statement as Statement
import AST.Type (Type)
import qualified AST.Type as Type

parseFile :: String -> Either String File
parseFile content = applyLeft show $ parse fileParser "<input>" content

---- AST.Declaration parsers ----

fileParser :: Parser File
fileParser = do
  _ <- anyWhitespace
  decls <- sepEndBy declarationParser statementSep
  _ <- anyWhitespace
  _ <- eof
  return decls

declarationParser :: Parser Declaraction
declarationParser = choice [letDeclaration, funcDeclaration, typeDeclaration]

letDeclaration :: Parser Declaraction
letDeclaration = do
  _ <- string "let"
  _ <- any1Whitespace
  name <- valueName
  _ <- any1Whitespace
  typ <- typeParser
  _ <- any1Whitespace
  _ <- char '='
  _ <- any1Whitespace
  val <- expressionParser
  return $ Declaraction.Let name typ val

funcDeclaration :: Parser Declaraction
funcDeclaration = do
  _ <- string "fn"
  _ <- any1LinearWhitespace
  name <- valueName
  _ <- char '('
  _ <- anyLinearWhitespace
  args <- funcArgDecl
  _ <- any1LinearWhitespace
  retType <- optionMaybe $ try $ do
    typ <- typeParser
    _ <- any1LinearWhitespace
    return typ
  body <- blockStatement
  let typ = Type.Function (map snd args) (unwrapOr retType Type.Nil)
  return $ Declaraction.Function name typ (map fst args) body

typeDeclaration :: Parser Declaraction
typeDeclaration = do
  _ <- string "type"
  _ <- any1LinearWhitespace
  name <- typeName
  _ <- any1LinearWhitespace
  typ <- typeParser
  return $ Declaraction.TypeDef name typ

funcArgDecl :: Parser [(String, Type)]
funcArgDecl = argDeclEnd <|> argDecl

argDeclEnd :: Parser [(String, Type)]
argDeclEnd = do
  _ <- char ')'
  return []

argDecl :: Parser [(String, Type)]
argDecl = do
  name <- valueName
  _ <- any1LinearWhitespace
  typ <- typeParser
  _ <- anyLinearWhitespace
  rest <- argDeclEnd <|> nextArgDecl
  return $ (name, typ) : rest

nextArgDecl :: Parser [(String, Type)]
nextArgDecl = do
  _ <- char ','
  _ <- anyLinearWhitespace
  argDecl

---- AST.Statement parsers ----

statementParser :: Parser Statement
statementParser = choice $ map try [
  returnStatement, letStatement, ifStatement, whileStatement, blockStatement,
  assignStatement, exprStatement]

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
  _ <- statementSep
  liftM Statement.Block $ blockStatements

blockStatements :: Parser [Statement]
blockStatements = endBlock <|> nextStatement

nextStatement :: Parser [Statement]
nextStatement = do
  stmt <- statementParser
  _ <- statementSep
  rest <- blockStatements
  return $ stmt : rest

endBlock :: Parser [Statement]
endBlock = do
  _ <- char '}'
  return []

statementSep :: Parser ()
statementSep = do
  _ <- anyLinearWhitespace
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
  _ <- anyWhitespace
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
  _ <- anyWhitespace
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

readBinExprParts :: Parser (Expression, [(BinOp, Expression)])
readBinExprParts = do
  e <- expr
  _ <- anyLinearWhitespace
  parts <- many $ try $ do
    op <- opParser
    _ <- anyLinearWhitespace
    e' <- expr
    _ <- anyLinearWhitespace
    return (op, e')
  return (e, parts)

unfoldParts :: (Expression, [(BinOp, Expression)]) -> Expression
unfoldParts bin =
  let (e, rest) = foldl unfoldOps bin precOrder
  in if rest /= [] then error "Unexpected operator"
     else e

unfoldOps :: (Expression, [(BinOp, Expression)]) -> [BinOp] ->
            (Expression, [(BinOp, Expression)])
unfoldOps (left, parts) ops = case parts of
  []              -> (left, [])
  ((op, right):pts) ->
    let (applied, rest) = unfoldOps (right, pts) ops
    in if elem op ops
       then (Expression.Binary op left applied, rest)
       else (left, (op, applied) : rest)

precOrder :: [[BinOp]]
precOrder =
  [ [Expression.Times, Expression.Divide, Expression.Mod]
  , [Expression.Plus, Expression.Minus]
  , [Expression.LShift, Expression.RShift, Expression.RRShift]
  , [Expression.Power]
  , [Expression.Less, Expression.LessEq, Expression.Greater, Expression.GreaterEq]
  , [Expression.Eq, Expression.NotEq]
  , [Expression.BitAnd]
  , [Expression.BitXor]
  , [Expression.BitOr]
  , [Expression.BoolAnd]
  , [Expression.BoolOr]
  ]

expr :: Parser Expression
expr = choice $ map try [parenExpr, valueExpr, unaryExpr, callExpr, castExpr, varExpr]

parenExpr :: Parser Expression
parenExpr = do
  _ <- char '('
  _ <- anyWhitespace
  ex <- expressionParser
  _ <- anyWhitespace
  _ <- char ')'
  return $ Expression.Paren ex

valueExpr :: Parser Expression
valueExpr = do
  val <- valueParser
  return $ Expression.Val val

unaryExpr :: Parser Expression
unaryExpr = do
  op <- unaryOpParser
  _ <- anyWhitespace
  ex <- expressionParser
  return $ Expression.Unary op ex

callExpr :: Parser Expression
callExpr = do
  fn <- varExpr
  _ <- char '('
  _ <- anyWhitespace
  args <- choice [fnCallArg, argsEnd]
  return $ Expression.Call fn args

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
  _ <- char ')'
  return []

castExpr :: Parser Expression
castExpr = do
  typ <- typeParser
  _ <- char '('
  _ <- anyWhitespace
  ex <- expressionParser
  _ <- anyWhitespace
  _ <- char ')'
  return $ Expression.Cast typ ex

varExpr :: Parser Expression
varExpr = do
  name <- valueName
  return $ Expression.Var name

--- parse values

valueParser :: Parser Value
valueParser = choice $ map try [structValueParser, stringParser, boolParser, numberParser]

structValueParser :: Parser Value
structValueParser = do
  typ <- typeName
  _ <- string "{"
  _ <- statementSep
  fields <- sepEndBy structFieldValue statementSep
  _ <- string "}"
  return $ Expression.StructVal typ fields

structFieldValue :: Parser (String, Expression)
structFieldValue = do
  field <- valueName
  _ <- string ":"
  _ <- anyLinearWhitespace
  value <- expressionParser
  _ <- string ","
  return (field, value)

stringParser :: Parser Value
stringParser = do
  s <- doubleQuotedString
  return $ Expression.StrVal s

boolParser :: Parser Value
boolParser = do
  b <- choices [("False", False), ("True", True)]
  return $ Expression.BoolVal b

--- parse floating and integer numbers

numberParser :: Parser Value
numberParser = do
  start <- digits
  choice [float start, integer start]

float :: String -> Parser Value
float start = do
  fp <- floatingPart
  return $ Expression.FloatVal (read (start ++ fp))

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
integer start = return $ Expression.IntVal (read start)

--- parse operators

opParser :: Parser BinOp
opParser = choices opChoices

opChoices :: [(String, BinOp)]
opChoices =
      [ ("+",  Expression.Plus)
      , ("-",  Expression.Minus)
      , ("**", Expression.Power)
      , ("*",  Expression.Times)
      , ("/",  Expression.Divide)
      , ("%",  Expression.Mod)
      , ("|",  Expression.BitOr)
      , ("^",  Expression.BitXor)
      , ("&&", Expression.BoolAnd)
      , ("&",  Expression.BitAnd)
      , ("||", Expression.BoolOr)
      , ("<<",  Expression.LShift)
      , (">>>", Expression.RRShift)
      , (">>",  Expression.RShift)
      , ("==", Expression.Eq)
      , ("!=", Expression.NotEq)
      , ("<=", Expression.LessEq)
      , ("<",  Expression.Less)
      , (">=", Expression.GreaterEq)
      , (">",  Expression.Greater)
      ]

unaryOpParser :: Parser UnaryOp
unaryOpParser =
  choices [ ("~", Expression.BitInvert)
          , ("!", Expression.BoolNot) ]

---- AST.Type parsers ----

typeParser :: Parser Type
typeParser = structTypeParser <|> choices types <|> namedType

types :: [(String, Type)]
types = [ ("String", Type.String)
        , ("Float", Type.Float)
        , ("Int", Type.Int)
        , ("Bool", Type.Bool)
        , ("()", Type.Nil)
        ]

structTypeParser :: Parser Type
structTypeParser = do
  _ <- string "struct"
  _ <- any1LinearWhitespace
  _ <- string "{"
  _ <- statementSep
  fields <- sepEndBy structField statementSep
  _ <- string "}"
  return $ Type.Struct fields

structField :: Parser (String, Type)
structField = do
  name <- valueName
  _ <- any1LinearWhitespace
  typ <- typeParser
  return (name, typ)

namedType :: Parser Type
namedType = do
  name <- typeName
  return $ Type.TypeName name

---- Helper functions ----

choices :: [(String, a)] -> Parser a
choices = choice . map try . map pair2parser

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
  case c of
   'n'  -> return "\n"
   't'  -> return "\t"
   '\\' -> return "\\"
   '"'  -> return "\""
   _    -> return $ '\\' : c : ""

typeName :: Parser String
typeName = do
  first <- upper
  rest <- many $ choice [letter, underscore]
  return $ first : rest

valueName :: Parser String
valueName = do
  first <- lower
  rest <- many $ choice [letter, digit, underscore]
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

linearWhitespaceCh :: Parser Char
linearWhitespaceCh = oneOf " \t"

anyLinearWhitespace :: Parser String
anyLinearWhitespace = many linearWhitespaceCh

any1LinearWhitespace :: Parser String
any1LinearWhitespace = many1 linearWhitespaceCh

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

applyLeft :: (a -> b) -> (Either a r) -> (Either b r)
applyLeft fn (Left  a) = Left (fn a)
applyLeft _  (Right r) = Right r
