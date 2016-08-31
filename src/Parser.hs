module Parser where

import Control.Monad ( liftM )
import Text.Parsec
import Text.Parsec.String (Parser)

import AST.Declaration (Declaration, File)
import qualified AST.Declaration as D
import AST.Expression (Expression, BinOp, UnaryOp, Value)
import qualified AST.Expression as E
import AST.Statement (Statement)
import qualified AST.Statement as S
import Type (Type)
import qualified Type as T

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

declarationParser :: Parser Declaration
declarationParser = choice [letDeclaration, funcDeclaration, typeDeclaration]

letDeclaration :: Parser Declaration
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
  return $ D.Let name typ val

funcDeclaration :: Parser Declaration
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
  let typ = T.Function (map snd args) (unwrapOr retType T.Nil)
  return $ D.Function name typ (map fst args) body

typeDeclaration :: Parser Declaration
typeDeclaration = do
  _ <- string "type"
  _ <- any1LinearWhitespace
  name <- typeName
  _ <- any1LinearWhitespace
  typ <- typeParser
  return $ D.TypeDef name typ

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
  return $ S.Return e

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
  return $ S.Let name typ val

assignStatement :: Parser Statement
assignStatement = do
  name <- valueName
  names <- try (assignFields [name]) <|> return [name]
  _ <- any1Whitespace
  _ <- char '='
  _ <- any1Whitespace
  val <- expressionParser
  return $ S.Assign names val

assignFields :: [String] -> Parser [String]
assignFields lefts = do
  _ <- char '.'
  right <- valueName
  let names = right : lefts
  try (assignFields names) <|> return (reverse names)

blockStatement :: Parser Statement
blockStatement = do
  _ <- char '{'
  _ <- statementSep
  liftM S.Block $ blockStatements

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
  return $ S.Expr e

ifStatement :: Parser Statement
ifStatement = do
  _ <- string "if"
  _ <- any1Whitespace
  test <- expressionParser
  _ <- anyWhitespace
  body <- blockStatement
  elsePart <- optionMaybe $ try elseBlock
  return $ let (S.Block stmts) = body
           in S.If test stmts elsePart

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
  return $ let (S.Block stmts) = body
           in S.While test stmts

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
       then (E.Binary op left applied, rest)
       else (left, (op, applied) : rest)

precOrder :: [[BinOp]]
precOrder =
  [ [E.Times, E.Divide, E.Mod]
  , [E.Plus, E.Minus]
  , [E.LShift, E.RShift, E.RRShift]
  , [E.Power]
  , [E.Less, E.LessEq, E.Greater, E.GreaterEq]
  , [E.Eq, E.NotEq]
  , [E.BitAnd]
  , [E.BitXor]
  , [E.BitOr]
  , [E.BoolAnd]
  , [E.BoolOr]
  ]

expr :: Parser Expression
expr = do
  e <- choice $ map try [parenExpr, valueExpr, unaryExpr, callExpr, castExpr, varExpr]
  try (accessExpr e) <|> return e

accessExpr :: Expression -> Parser Expression
accessExpr left = do
  _ <- char '.'
  right <- valueName
  let e = E.Access left right
  try (accessExpr e) <|> return e

parenExpr :: Parser Expression
parenExpr = do
  _ <- char '('
  _ <- anyWhitespace
  ex <- expressionParser
  _ <- anyWhitespace
  _ <- char ')'
  return $ E.Paren ex

valueExpr :: Parser Expression
valueExpr = do
  val <- valueParser
  return $ E.Val val

unaryExpr :: Parser Expression
unaryExpr = do
  op <- unaryOpParser
  _ <- anyWhitespace
  ex <- expressionParser
  return $ E.Unary op ex

callExpr :: Parser Expression
callExpr = do
  fn <- varExpr
  _ <- char '('
  _ <- anyWhitespace
  args <- choice [fnCallArg, argsEnd]
  return $ E.Call fn args

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
  return $ E.Cast typ ex

varExpr :: Parser Expression
varExpr = do
  name <- valueName
  return $ E.Var name

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
  return $ E.StructVal typ fields

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
  return $ E.StrVal s

boolParser :: Parser Value
boolParser = do
  b <- choices [("False", False), ("True", True)]
  return $ E.BoolVal b

--- parse floating and integer numbers

numberParser :: Parser Value
numberParser = do
  start <- digits
  choice [float start, integer start]

float :: String -> Parser Value
float start = do
  fp <- floatingPart
  return $ E.FloatVal (read (start ++ fp))

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
integer start = return $ E.IntVal (read start)

--- parse operators

opParser :: Parser BinOp
opParser = choices opChoices

opChoices :: [(String, BinOp)]
opChoices =
      [ ("+",  E.Plus)
      , ("-",  E.Minus)
      , ("**", E.Power)
      , ("*",  E.Times)
      , ("/",  E.Divide)
      , ("%",  E.Mod)
      , ("|",  E.BitOr)
      , ("^",  E.BitXor)
      , ("&&", E.BoolAnd)
      , ("&",  E.BitAnd)
      , ("||", E.BoolOr)
      , ("<<",  E.LShift)
      , (">>>", E.RRShift)
      , (">>",  E.RShift)
      , ("==", E.Eq)
      , ("!=", E.NotEq)
      , ("<=", E.LessEq)
      , ("<",  E.Less)
      , (">=", E.GreaterEq)
      , (">",  E.Greater)
      ]

unaryOpParser :: Parser UnaryOp
unaryOpParser =
  choices [ ("~", E.BitInvert)
          , ("!", E.BoolNot) ]

---- Type parsers ----

typeParser :: Parser Type
typeParser = enumTypeParser <|> structTypeParser <|> choices types <|> namedType

types :: [(String, Type)]
types = [ ("String", T.String)
        , ("Float", T.Float)
        , ("Int", T.Int)
        , ("Bool", T.Bool)
        , ("()", T.Nil)
        ]

enumTypeParser :: Parser Type
enumTypeParser = do
  _ <- string "enum"
  _ <- any1LinearWhitespace
  _ <- string "{"
  _ <- statementSep
  options <- sepEndBy enumField statementSep
  _ <- string "}"
  return $ T.Enum options

enumField :: Parser (String, [(String, Type)])
enumField = do
  name <- typeName
  fields <- optionMaybe $ try $ do
    _ <- any1LinearWhitespace
    structTypeBody
  return (name, unwrapOr fields [])

structTypeParser :: Parser Type
structTypeParser = do
  _ <- string "struct"
  _ <- any1LinearWhitespace
  fields <- structTypeBody
  return $ T.Struct fields

structTypeBody :: Parser [(String, Type)]
structTypeBody = do
  _ <- string "{"
  _ <- statementSep
  fields <- sepEndBy structField statementSep
  _ <- string "}"
  return fields

structField :: Parser (String, Type)
structField = do
  name <- valueName
  _ <- any1LinearWhitespace
  typ <- typeParser
  return (name, typ)

namedType :: Parser Type
namedType = do
  name <- typeName
  return $ T.TypeName name

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
