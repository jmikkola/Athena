module Backends.Go.Emit where

import Control.Monad.State

import Backends.Go.Syntax (
  Type (..),
  FunctionDecl (..),
  NamedT (..),
  Declaration (..),
  Statement (..),
  Expression (..),
  UnaryOp (..),
  BinaryOp (..) )

data Output = Output { indent :: Int
                     , pieces :: [String]
                     }
type Result = Either String
type EmitState = StateT Output Result

write :: String -> EmitState ()
write s = modify (\o -> o { pieces = (s : pieces o) })

readAll :: EmitState String
readAll = do
  output <- get
  return $ concat $ reverse $ pieces output

getIndent :: EmitState Int
getIndent = do
  o <- get
  return $ indent o

alterIndent :: Int -> EmitState ()
alterIndent delta = modify (\o -> o { indent = delta + indent o })

increaseIndent :: EmitState ()
increaseIndent = alterIndent 1

decreateIndent :: EmitState ()
decreateIndent = alterIndent (-1)

writeIndent :: EmitState ()
writeIndent = do
  n <- getIndent
  write $ indentString n

indentString :: Int -> String
indentString n = replicate n '\t'

--- Actual Emitters ---

emitFile :: [Declaration] -> Either String String
emitFile file = evalStateT (emitFileM file) (Output 0 [])

emitFileM :: [Declaration] -> EmitState String
emitFileM decls = do
  write "package main\n\n"
  emitImports [("fmt", "fmt.Println"), ("math", "math.Pow")]
  intersperse (write "\n\n") (map emitDeclaration decls)
  readAll

emitImports :: [(String, String)] -> EmitState ()
emitImports imps = do
  write "import ("
  _ <- mapM (\i -> write $ "\n\t\"" ++ i ++ "\"") $ map fst imps
  write "\n)\n"
  _ <- mapM (\fn -> write $ "\nvar _ = " ++ fn) $ map snd imps
  write "\n\n"

emitType :: Type -> EmitState ()
emitType t = case t of
  GoInt64
    -> write "int64"
  GoInt
    -> write "int"
  GoFloat64
    -> write "float64"
  GoBool
    -> write "bool"
  GoString
    -> write "string"
  GoChar
    -> write "rune"
  GoFunc args rets
    -> do
      write "func ("
      intersperse (write ", ") (map emitType args)
      write ")"
      case rets of
       []
         -> return ()
       [GoVoid]
         -> return ()
       [t']
         -> do
           write " "
           emitType t'
       _
         -> do
           write " ("
           intersperse (write ", ") (map emitType rets)
           write ")"
  GoStruct name
    -> do
      write "*"
      write name
  GoInterface name
    -> write name
  GoVoid
    -> return ()
  TypeName name
    -> write name

emitFunctionDecl :: FunctionDecl -> EmitState ()
emitFunctionDecl (FunctionDecl args rets) = do
  write "("
  intersperse (write ", ") (map emitNamedT args)
  write ")"
  writeReturn rets

writeReturn :: [NamedT] -> EmitState ()
writeReturn []
  = return ()
writeReturn [JustType GoVoid]
  = return ()
writeReturn typs
  = let inner = intersperse (write ", ") (map emitNamedT typs)
    in if retNeedsParens typs
       then do
         write " ("
         inner
         write ")"
       else do
         write " "
         inner
         return ()

retNeedsParens :: [NamedT] -> Bool
retNeedsParens []
  = False
retNeedsParens [NamedType _ _]
  = True
retNeedsParens [_]
  = False
retNeedsParens _
  = True

emitNamedT :: NamedT -> EmitState ()
emitNamedT (JustName name)
  = write name
emitNamedT (JustType t)
  = emitType t
emitNamedT (NamedType name t)
  = do
   write name
   write " "
   emitType t

emitDeclaration :: Declaration -> EmitState ()
emitDeclaration decl = case decl of
  Structure name fields
    -> do
      write "type "
      write name
      write " struct "
      linesInBlock emitFieldType fields
  Interface name methods
    -> do
      write "type "
      write name
      write " interface "
      linesInBlock emitMethod methods
  Variable name mType expr
    -> do
      write "var "
      write name
      case mType of
        Nothing -> return ()
        Just tp -> do
          write " "
          emitType tp
      write " = "
      emitExpression expr
  Constant name expr
    -> do
      write "const "
      write name
      write " = "
      emitExpression expr
  Method (recvName, recvType) name fnDecl body
    -> do
      write "func ("
      write recvName
      write ", "
      emitType recvType
      write ") "
      write name
      emitFunctionDecl fnDecl
      write " "
      emitStatement body
  Function name fnDecl body
    -> do
      write "func "
      write name
      emitFunctionDecl fnDecl
      write " "
      emitStatement body

emitFieldType :: (String, Type) -> EmitState ()
emitFieldType (name, typ) = do
  write name
  write " "
  emitType typ

emitMethod :: (String, FunctionDecl) -> EmitState ()
emitMethod (name, decl) = do
  write name
  emitFunctionDecl decl

emitStatement :: Statement -> EmitState ()
emitStatement stmt = do
  writeIndent
  emitStatementNoIndent stmt

emitStatementNoIndent :: Statement -> EmitState ()
emitStatementNoIndent stmt = case stmt of
  JustReturn
    -> write "return"
  Return ex
    -> do
      write "return "
      emitExpression ex
  VarStmt var (Just typ) ex
    -> do
      write "var "
      write var
      emitType typ
      write " = "
      emitExpression ex
  VarStmt var Nothing ex
    -> do
      write var
      write " := "
      emitExpression ex
  Assign names ex
    -> do
      intersperse (write ".") (map write names)
      write " = "
      emitExpression ex
  Expr ex
    -> emitExpression ex
  If test body mElse
    -> do
      write "if "
      emitExpression test
      write " "
      emitStatementNoIndent body
      emitElse mElse
  Loop body
    -> do
      write "for "
      emitStatementNoIndent body
  For1 test body
    -> do
      write "for "
      emitExpression test
      write " "
      emitStatementNoIndent body
  For3 mStart test mStep body
    -> do
      write "for "
      case mStart of
        Nothing -> return ()
        Just st -> emitStatementNoIndent st
      write ";"
      emitExpression test
      write ";"
      case mStep of
        Nothing -> return ()
        Just st -> emitStatementNoIndent st
      write " "
      emitStatementNoIndent body
  Block stmts ->
    linesInBlock emitStatement stmts

emitElse :: Maybe Statement -> EmitState ()
emitElse els = case els of
  Nothing -> return ()
  Just st -> do
    write " "
    emitStatementNoIndent st

emitExpression :: Expression -> EmitState ()
emitExpression expr = case expr of
  Paren e
    -> do
      write "("
      emitExpression e
      write ")"
  Unary op e
    -> do
      emitUnaryOp op
      emitExpression e
  Binary op l r
    -> do
      emitExpression l
      write " "
      write (showBinaryOp op)
      write " "
      emitExpression r
  Call fn args
    -> do
      emitExpression fn
      write "("
      intersperse (write ", ") (map emitExpression args)
      write ")"
  InterfaceCast iface ex
    -> do
      write "("
      emitExpression ex
      write ").("
      emitType iface
      write ")"
  TypeCast typ ex
    -> do
       emitType typ
       write "("
       emitExpression ex
       write ")"
  Var name
    -> write name
  FieldAccess ex field
    -> do
       emitExpression ex
       write "."
       write field
  ArrayAccess ex fieldEx
    -> do
      emitExpression ex
      write "["
      emitExpression fieldEx
      write "]"
  Func typ argNames body
    -> emitFunc typ argNames body
  StrVal s
    -> write $ show s
  BoolVal b
    -> write (if b then "true" else "false")
  IntVal i
    -> write $ show i
  FloatVal f
    -> write $ show f
  StructVal name fields
    -> do
       write name
       write " "
       linesInBlock emitStructField fields

emitFunc :: Type -> [String] -> Statement -> EmitState ()
emitFunc (GoFunc ats rts) argNames body = do
  write "func ("
  intersperse (write ", ") (map emitArg $ zip argNames ats)
  write ")"
  writeReturn $ map JustType rts
  emitStatement body
emitFunc t _ _ = error $ "compiler error: bad function type " ++ show t

emitArg :: (String, Type) -> EmitState ()
emitArg (name, typ) = do
  write name
  write " "
  emitType typ

linesInBlock :: (a -> EmitState b) -> [a] -> EmitState ()
linesInBlock fn items = inBlock $ do
  intersperse (write "\n") (map (void . fn) items)
  write "\n"

inBlock :: EmitState a -> EmitState ()
inBlock f = do
  write "{\n"
  increaseIndent
  _ <- f
  decreateIndent
  writeIndent
  write "}"

emitStructField :: (String, Expression) -> EmitState ()
emitStructField (name, ex) = do
  writeIndent
  write name
  write ": "
  emitExpression ex
  write ",\n"

emitUnaryOp :: UnaryOp -> EmitState ()
emitUnaryOp BitInvert
  = write "^"
emitUnaryOp BoolNot
  = write "!"

showBinaryOp :: BinaryOp -> String
showBinaryOp op = case op of
  Plus -> "+"
  Minus -> "-"
  Times -> "*"
  Divide -> "/"
  Mod -> "%"
  BitAnd -> "&"
  BitOr -> "|"
  BitXor -> "^"
  BoolAnd -> "&&"
  BoolOr -> "||"
  Eq -> "=="
  NotEq -> "!="
  Less -> "<"
  LessEq -> "<="
  Greater -> ">"
  GreaterEq -> ">="
  LShift -> "<<"
  RShift -> ">>"
  RRShift -> ">>>"

intersperse :: EmitState () -> [EmitState ()] -> EmitState ()
intersperse sep lst =
  case lst of
   (a1:a2:rest) -> do
     a1
     sep
     intersperse sep (a2:rest)
   [a]          -> a
   []           -> return ()
