module Parse ( Display (..)
             , FunctionDef (..)
             , FnArg (..)
             , TypeDef (..)
             , Block (..)
             , ElsePart (..)
             , Statement (..)
             , BinaryOp (..)
             , UnaryOp (..)
             , Expression (..)
             , LiteralValue (..)
             , FunctionName
             , VariableName
             , parseFile
             , fnName
             , getArgs
             , fnArgNames
             , anyWhitespace
             , any1Whitespace
             , digits
             , literal
             , doubleQuotedString
             , hexNum
             , octalNum
             , expression
             , binaryExpression
             , functionCallExpression
             , fnCallArgs
             , statement
             , ifStatement
             , block
             , typeDef
             , functionDef
             , parseLineComment
             , parseBlockComment
             , parseComment
             ) where

import Control.Monad ( liftM, foldM )
import Data.Char ( digitToInt )
import Data.List ( intercalate )

import Data.Map (Map)
import qualified Data.Map as Map

import Text.Parsec
import Text.Parsec.String (Parser)

class Display a where
  display :: a -> String

data FunctionDef = FunctionDef String [FnArg] (Maybe TypeDef) Block
                 | ShortFn String [FnArg] (Maybe TypeDef) Expression
                 deriving (Eq, Show)

fnName :: FunctionDef -> String
fnName (FunctionDef name _ _ _) = name
fnName (ShortFn     name _ _ _) = name

getArgs :: FunctionDef -> [FnArg]
getArgs (FunctionDef _ args _ _) = args
getArgs (ShortFn     _ args _ _) = args

fnArgNames :: FunctionDef -> [String]
fnArgNames = (map argName) . getArgs

instance Display FunctionDef where
  display (FunctionDef name args maybeType blk) =
    "fn " ++ name ++ displayInParens args ++ " " ++ displayRet maybeType ++ display blk
  display (ShortFn name args maybeType expr) =
    "fn " ++ name ++ displayInParens args ++ " " ++ displayRet maybeType ++ "= " ++ display expr

displayRet :: Maybe TypeDef -> String
displayRet Nothing  = ""
displayRet (Just t) = display t ++ " "

data FnArg = FnArg { argName :: String, argType :: (Maybe TypeDef) }
             deriving (Eq, Show)

instance Display FnArg where
  display (FnArg name maybeType) = name ++ rest
    where rest = case maybeType of
            Nothing -> ""
            Just t  -> " " ++ display t

data TypeDef = NilType | NamedType String [TypeDef] | TypeVar String
             deriving (Eq, Show)

instance Display TypeDef where
  display  NilType               = "()"
  display (NamedType s [])       = s
  display (NamedType s subtypes) = s ++ "[" ++ displayCommaSep subtypes ++ "]"
  display (TypeVar   s)          = s

data Block = Block [Statement]
           deriving (Eq, Show)

instance Display Block where
  display (Block statements) = "{\n" ++ (concatMap display statements) ++ "}\n"

data ElsePart = NoElse | Else Block | ElseIf Expression Block ElsePart
              deriving (Eq, Show)

instance Display ElsePart where
  display NoElse = ""
  display (Else blk) = "else " ++ display blk
  display (ElseIf expr blk ep) = "else if " ++ display expr ++ " " ++ display blk ++ display ep

-- TODO: add type declarations and constants
data Statement = StatementExpr Expression
               | StatementLet VariableName Expression
               | StatementAssign VariableName Expression
               | StatementReturn Expression
               | StatementIf Expression Block ElsePart
               | StatementWhile Expression Block
               | StatementFor VariableName Expression Block
               | StatementFn FunctionDef
               deriving (Show, Eq)

instance Display Statement where
  display (StatementExpr expr) = display expr ++ "\n"
  display (StatementLet var expr) = "let " ++ var ++ " = " ++ display expr ++ "\n"
  display (StatementAssign var expr) = var ++ " = " ++ display expr ++ "\n"
  display (StatementReturn expr) = "return " ++ display expr ++ "\n"
  display (StatementIf test blk ep) = "if " ++ display test ++ " " ++ display blk ++ display ep
  display (StatementWhile test blk) = "while " ++ display test ++ " " ++ display blk
  display (StatementFor var expr blk) = "for " ++ var ++ " in " ++ display expr ++ " " ++ display blk
  display (StatementFn funcDef) = display funcDef

data BinaryOp = Plus | Minus | Times | Divide | Mod | Power
              | Less | LessEq | Equals | Greater | GreaterEq | NotEq
              | And | Or
              deriving (Show, Eq)

instance Display BinaryOp where
  display Plus      = "+"
  display Minus     = "-"
  display Times     = "*"
  display Divide    = "/"
  display Mod       = "%"
  display Power     = "^"
  display Less      = "<"
  display LessEq    = "<="
  display Equals    = "=="
  display Greater   = ">"
  display GreaterEq = ">="
  display NotEq     = "!="
  display And       = "&&"
  display Or        = "||"

data UnaryOp = Negate | Flip | Not
             deriving (Show, Eq)

instance Display UnaryOp where
  display Negate = "-"
  display Flip   = "~"
  display Not    = "!"

data Expression = ExpressionLit LiteralValue
                | ExpressionVar VariableName
                | ExpressionParen Expression
                | ExpressionFnCall FunctionName [Expression]
                | ExpressionBinary BinaryOp Expression Expression
                | ExpressionUnary UnaryOp Expression
                | ExpressionStruct String [Expression]
                | ExpressionIf Expression Expression Expression
                deriving (Show, Eq)

interleave :: [a] -> [a] -> [a]
interleave (a:as) b = a : interleave b as
interleave []     _ = []

instance Display Expression where
  display (ExpressionLit    lv)       = display lv
  display (ExpressionVar    varName)  = varName
  display (ExpressionParen  inner)    = "(" ++ display inner ++ ")"
  display (ExpressionFnCall fn args)  = fn ++ displayInParens args
  display (ExpressionBinary op l r)   = display l ++ " " ++ display op ++ " " ++ display r
  display (ExpressionUnary  op inner) = display op ++ display inner
  display (ExpressionStruct s  exprs) = if null exprs then s else s ++ displayInParens exprs
  display (ExpressionIf test tr fl)   =
    concat $ interleave ["if ", " then ", " else "] (map display [test, tr, fl])

displayInParens :: (Display a) => [a] -> String
displayInParens ds = "(" ++ displayCommaSep ds ++ ")"

displayCommaSep :: (Display a) => [a] -> String
displayCommaSep = (intercalate ", ") . (map display)

data LiteralValue = LiteralFloat Float | LiteralInt Int | LiteralString String | LiteralChar Char
                  deriving (Show, Eq)

instance Display LiteralValue where
  display (LiteralFloat  f) = show f
  display (LiteralInt    i) = show i
  display (LiteralString s) = show s
  display (LiteralChar   c) = show c

type VariableName = String
type FunctionName = String

unwrapOr :: Maybe a -> a -> a
unwrapOr (Just a) _ = a
unwrapOr Nothing  b = b

maybeEmpty :: Maybe String -> String
maybeEmpty m = unwrapOr m ""

parseFile :: Parser [Statement]
parseFile = do
  _ <- anyWhitespace
  statement `sepEndBy` any1Whitespace

-- Statements starting with a keyword are safe to "try" because they will fail fast
statement :: Parser Statement
statement = choice [ try letStatement
                   , try returnStatement
                   , try ifStatement
                   , try whileStatement
                   , try forStatement
                   , try assignmentStatement
                   , try fnStatement
                   , expressionStatment
                   ]

expressionStatment :: Parser Statement
expressionStatment = liftM StatementExpr $ expression

letStatement :: Parser Statement
letStatement = do
  _ <- letKwd
  _ <- any1LinearWhitespace
  var <- valueName
  _ <- any1LinearWhitespace
  _ <- char '='
  _ <- any1Whitespace
  expr <- expression
  return $ StatementLet var expr

assignmentStatement :: Parser Statement
assignmentStatement = do
  var <- valueName
  _ <- any1LinearWhitespace
  _ <- char '='
  _ <- any1Whitespace
  expr <- expression
  return $ StatementAssign var expr

returnStatement :: Parser Statement
returnStatement = do
  _ <- returnKwd
  _ <- any1LinearWhitespace
  expr <- expression
  return $ StatementReturn expr

fnStatement :: Parser Statement
fnStatement = liftM StatementFn $ functionDef

ifStatement :: Parser Statement
ifStatement = do
  _ <- ifKwd
  _ <- any1LinearWhitespace
  test <- expression
  _ <- any1LinearWhitespace
  body <- block
  elsePart <- maybeElse
  return $ StatementIf test body elsePart

maybeElse :: Parser ElsePart
maybeElse = do
  elsePart <- optionMaybe $ try elseBlock
  return $ unwrapOr elsePart NoElse

elseBlock :: Parser ElsePart
elseBlock = do
  _ <- anyWhitespace
  _ <- elseKwd
  _ <- any1LinearWhitespace
  elseIfStmt <|> elseStmt <?> "else block"

elseIfStmt :: Parser ElsePart
elseIfStmt = do
  _ <- ifKwd
  _ <- any1Whitespace
  test <- expression
  _ <- any1LinearWhitespace
  body <- block
  elsePart <- maybeElse
  return $ ElseIf test body elsePart

elseStmt :: Parser ElsePart
elseStmt = do
  body <- block
  return $ Else body

whileStatement :: Parser Statement
whileStatement = do
  _ <- whileKwd
  _ <- any1LinearWhitespace
  test <- expression
  _ <- any1LinearWhitespace
  body <- block
  return $ StatementWhile test body

forStatement :: Parser Statement
forStatement = do
  _ <- forKwd
  _ <- any1LinearWhitespace
  loopVar <- valueName
  _ <- any1LinearWhitespace
  _ <- inKwd
  _ <- any1LinearWhitespace
  expr <- expression
  _ <- any1LinearWhitespace
  body <- block
  return $ StatementFor loopVar expr body

block :: Parser Block
block = do
  startBlock
  liftM Block $ blockStatements

startBlock :: Parser ()
startBlock = do
  _ <- char '{'
  _ <- anyWhitespace
  return ()

blockStatements :: Parser [Statement]
blockStatements = blockStatement <|> endBlock

-- TODO: fix this mess
blockStatement :: Parser [Statement]
blockStatement = do
  stmt <- statement
  _ <- anyLinearWhitespace
  rest <- choice [ endBlock
                , do
                  _ <- statementSep
                  _ <- anyWhitespace
                  blockStatements
                ]
  return $ stmt : rest

endBlock :: Parser [Statement]
endBlock = do
  _ <- char '}'
  return []

expression :: Parser Expression
expression = binaryExpression

nonBinaryExpression ::  Parser Expression
nonBinaryExpression = choice [ parenExpression
                             , unaryExpr
                             , structExpression
                             , try literalExpression
                             , try ifExpression
                             , lowerLetterExpr
                             ]

ifExpression :: Parser Expression
ifExpression = do
  _ <- ifKwd
  _ <- any1LinearWhitespace
  test <- expression
  _ <- any1Whitespace
  _ <- thenKwd
  _ <- any1Whitespace
  ifCase <- expression
  _ <- any1Whitespace
  _ <- elseKwd
  _ <- any1Whitespace
  elseCase <- expression
  return $ ExpressionIf test ifCase elseCase

-- Expressions that start with a lowercase letter
lowerLetterExpr :: Parser Expression
lowerLetterExpr = do
  name <- valueName
  choice [try $ functionCallExpression name, variableExpression name]

literalExpression :: Parser Expression
literalExpression = liftM ExpressionLit $ literal

literal :: Parser LiteralValue
literal = choice [try hexLiteral, try octalLiteral, numericLiteral, stringLiteral, charLiteral]

charLiteral :: Parser LiteralValue
charLiteral = do
  _ <- char '\''
  ch <- noneOf ['\\', '\''] <|> escapedCh
  _ <- char '\''
  return $ LiteralChar ch

escapedCh :: Parser Char
escapedCh = do
  _ <- char '\\'
  ch <- anyChar
  return $ escapeReplace ch

escapeReplace :: Char -> Char
escapeReplace ch = case Map.lookup ch escapeCodes of
  Nothing -> ch
  Just c' -> c'

escapeCodes :: Map Char Char
escapeCodes = Map.fromList [ ('n', '\n')
                           , ('r', '\r')
                           , ('t', '\t')
                           ]

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

structExpression :: Parser Expression
structExpression = do
  name <- typeName
  args <- optionMaybe fnCallArgs
  return $ ExpressionStruct name (unwrapOr args [])

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

binaryOpLevels :: [[BinaryOp]]
binaryOpLevels = [ [LessEq, GreaterEq, NotEq, Equals, Less, Greater]
                 , [Plus, Minus, And, Or]
                 , [Times, Divide, Mod]
                 , [Power]
                 ]

binaryOpLevel :: [[BinaryOp]] -> Parser Expression
binaryOpLevel (l:ls) = binExprCurr
  where binExprNext = binaryOpLevel ls
        binExprCurr = choice [ try $ binaryExpressionLevel binExprCurr binExprNext l
                             , binExprNext
                             ]
binaryOpLevel []     = nonBinaryExpression


binaryExpression :: Parser Expression
binaryExpression = binaryOpLevel binaryOpLevels

binOp :: BinaryOp -> Parser BinaryOp
binOp op = do
  _ <- string $ display op
  return op

binaryExpressionLevel :: Parser Expression -> Parser Expression -> [BinaryOp] -> Parser Expression
binaryExpressionLevel self nextLevel ops = do
  left <- nextLevel
  _ <- try anyWhitespace
  op <- choice $ map (try . binOp) ops
  _ <- try anyWhitespace
  right <- self
  return $ ExpressionBinary op left right

unaryOp :: Parser UnaryOp
unaryOp = choice $ map (try . uOp) [Negate, Flip, Not]
  where uOp op = do
          _ <- string $ display op
          return op

unaryExpr :: Parser Expression
unaryExpr = do
  op <- unaryOp
  expr <- nonBinaryExpression
  return $ ExpressionUnary op expr

typeDef :: Parser TypeDef
typeDef = nilType <|> namedType <|> typeVar <?> "type def"

nilType :: Parser TypeDef
nilType = do
  _ <- string "()"
  return NilType

namedType :: Parser TypeDef
namedType = do
  name <- typeName
  params <- optionMaybe typeParams
  return $ NamedType name (unwrapOr params [])

typeParams :: Parser [TypeDef]
typeParams = do
  _ <- char '['
  _ <- anyLinearWhitespace
  params <- typeDef `sepBy` (do
                               _ <- char ','
                               anyLinearWhitespace)
  _ <- anyLinearWhitespace
  _ <- char ']'
  return params

typeVar :: Parser TypeDef
typeVar = do
  varName <- many1 lower
  return $ TypeVar varName

functionDef :: Parser FunctionDef
functionDef = try functionDefLong <|> functionDefShort <?> "function definition"

fnStart :: Parser (String, [FnArg], Maybe TypeDef)
fnStart = do
  _ <- fnKwd
  _ <- any1LinearWhitespace
  fnName <- valueName
  args <- fnArgs
  _ <- any1LinearWhitespace
  retType <- optionMaybe $ do
    retType <- typeDef
    _ <- any1LinearWhitespace
    return retType
  return (fnName, args, retType)

functionDefLong :: Parser FunctionDef
functionDefLong = do
  (fnName, args, retType) <- fnStart
  body <- block
  return $ FunctionDef fnName args retType body

functionDefShort :: Parser FunctionDef
functionDefShort = do
  (fnName, args, retType) <- fnStart
  _ <- char '='
  _ <- any1Whitespace
  body <- expression
  return $ ShortFn fnName args retType body

fnArgs :: Parser [FnArg]
fnArgs = do
  _ <- char '('
  _ <- anyWhitespace
  nextFnArg

nextFnArg :: Parser [FnArg]
nextFnArg = fnArgEnd <|> do
  arg <- fnArg
  _ <- anyWhitespace
  rest <- fnArgEnd <|> do
    _ <- char ','
    _ <- any1Whitespace
    nextFnArg
  return $ arg : rest

fnArgEnd :: Parser [FnArg]
fnArgEnd = do
  _ <- char ')'
  return []

fnArg :: Parser FnArg
fnArg = do
  name <- valueName
  argType <- optionMaybe $ do
    _ <- any1LinearWhitespace
    typeDef
  return $ FnArg name argType

fnKwd :: Parser String
fnKwd = string "fn"

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

thenKwd :: Parser String
thenKwd = string "then"

returnKwd :: Parser String
returnKwd = string "return"

whitespaceChs :: String
whitespaceChs = " \t\r\n"

underscore :: Parser Char
underscore = char '_'

anyWhitespaceCh :: Parser Char
anyWhitespaceCh = oneOf whitespaceChs

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

linearWhitespaceCh :: Parser Char
linearWhitespaceCh = oneOf " \t"

anyLinearWhitespace :: Parser String
anyLinearWhitespace = many linearWhitespaceCh

any1LinearWhitespace:: Parser String
any1LinearWhitespace = many1 linearWhitespaceCh

statementSep :: Parser String
statementSep = choice [string "\n", string ";"]

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
