module Emit where

import Control.Monad.Writer

import AST.Declaration (Declaraction, File)
import qualified AST.Declaration as D
import AST.Expression (Expression, Value, UnaryOp, BinOp)
import qualified AST.Expression as E
import AST.Statement (Statement)
import qualified AST.Statement as S
import AST.Type (Type)
import qualified AST.Type as T

class Render a where
  render :: a -> String

class Emitter a where
  emit :: a -> Writer String ()

instance Emitter Value where
  emit (E.EString s)   =
    tell $ show s
  emit (E.EBool b)     =
    tell $ if b then "true" else "false"
  emit (E.EInt i)      =
    tell $ show i
  emit (E.EFloat f)    =
    tell $ show f
  emit (E.EStruct t f) = do
    tell "&"
    tell t
    tell "{\n"
    _ <- mapM (\(fname, fexpr) -> do { tell fname; tell ": "; emit fexpr; tell ",\n" }) f
    tell "}"

instance Emitter Expression where
  emit (E.EParen e)        = do
    tell "("
    emit e
    tell ")"
  emit (E.EValue v)        =
    emit v
  emit (E.EUnary op e)     = do
    emit op
    emit e
  emit (E.EBinary op l r)  = do
    case op of
     E.Power -> do
       tell "math.Pow("
       emit l
       tell ", "
       emit r
       tell ")"
     _       -> do
       emit l
       tell " "
       emit op
       tell " "
       emit r
  emit (E.ECall fex argex) = do
    emit fex
    tell "("
    tellList argex emit
    tell ")"
  emit (E.ECast t e)       =
    case t of
     T.String -> do
       tell "fmt.Sprintf(\"%v\", "
       emit e
       tell ")"
     T.Nil    ->
       fail "compiler bug: Can't cast to nil"
     _        -> do
       emit t
       tell "("
       emit e
       tell ")"
  emit (E.EVariable v)     =
    if v == "print"
    then tell "fmt.Println"
    else tell v

instance Render UnaryOp where
  render E.BitInvert = "^"
  render E.BoolNot   = "!"

instance Emitter UnaryOp where
  emit = tell . render

instance Render BinOp where
  render E.Plus      = "+"
  render E.Minus     = "-"
  render E.Times     = "*"
  render E.Divide    = "/"
  render E.Mod       = "%"
  render E.Power     = "**"
  render E.BitAnd    = "&"
  render E.BitOr     = "|"
  render E.BitXor    = "/* TODO: no direct xor in go */"
  render E.BoolAnd   = "&&"
  render E.BoolOr    = "||"
  render E.Eq        = "=="
  render E.NotEq     = "!="
  render E.Less      = "<"
  render E.LessEq    = "<="
  render E.Greater   = ">"
  render E.GreaterEq = ">="
  render E.LShift    = "<<"
  render E.RShift    = ">>"
  render E.RRShift   = ">>>"

instance Emitter BinOp where
  emit = tell . render

instance Emitter Type where
  emit T.Nil    = tell ""
  emit T.Int    = tell "int64"
  emit T.Float  = tell "float64"
  emit T.Bool   = tell "bool"
  emit T.String = tell "string"
  emit (T.Function args ret) = do
    tell "func ("
    tellList args emit
    tell ")"
    if ret /= T.Nil then tell " " else return ()
    emit ret
  -- TODO: do type resolution before this?
  emit (T.TypeName name) = do
    tell "*" -- avoid infinite types
    tell name
  emit (T.Struct fields) = do
    tell "struct {\n"
    _ <- mapM (\(fname, ftype) -> do { tell fname; tell " "; emit ftype; tell "\n" }) fields
    tell "}"

instance Emitter Statement where
  emit (S.Return me) = do
    tell "return"
    case me of
     Nothing  -> return ()
     (Just e) -> do
       tell " "
       emit e
  emit (S.Let name t e) = do
    -- conveniently, both languages require a "let"-like statment
    tell "var "
    tell name
    tell " "
    emit t
    tell " = "
    emit e
    -- get around "declared and not used" issues
    tell "\nvar _ = "
    tell name
  emit (S.Assign name e) = do
    tell name
    tell " = "
    emit e
  emit (S.Block stmts) = do
    -- TODO: indent
    tell "{\n"
    _ <- mapM (\stmt -> do {emit stmt; tell "\n"}) stmts
    tell "}"
  emit (S.Expr e) =
    emit e
  emit (S.If test body melse) = do
    tell "if "
    emit test
    tell " "
    emit (S.Block body)
    case melse of
     Nothing    -> return ()
     (Just els) -> do
       tell " else "
       emit els
  emit (S.While test body) = do
    tell "for "
    emit test
    tell " "
    emit (S.Block body)

instance Emitter Declaraction where
  emit (D.Let name t e) = do
    tell "var "
    tell name
    tell " "
    emit t
    tell " = "
    emit e
  emit (D.Function name t args body) = do
    tell "func "
    tell name
    tell "("
    case t of
     (T.Function argTypes retType) -> do
       tellList (zip args argTypes) (\(a,t') -> do {tell a; tell " "; emit t'})
       tell ")"
       case retType of
        T.Nil -> return ()
        _     -> do
          tell " "
          emit retType
     _ -> error "compiler bug, function is wrong type"
    tell " "
    emit body
  emit (D.TypeDef name typ) = do
    tell "type "
    tell name
    tell " "
    emit typ
    case typ of
     (T.Struct fields) -> emitStructImpls name fields
     _                 -> return ()

-- Once interface implementations exist, generate these in a separate phase
-- Example output:
{-
func (self *Foo) String() {
return fmt.Sprintf("Foo{\nbar: %v,\nzap: %v,\n}", self.bar, self.zap)
}
-}
emitStructImpls :: String -> [(String, Type)] -> Writer String ()
emitStructImpls name fields = do
  let fieldNames = map fst fields
  tell "\n\n"
  tell "func (self *"
  tell name
  tell ") String() string {\n"
  tell "return fmt.Sprintf(\""
  tell name
  tell "{\\n"
  _ <- mapM (\fname -> do {tell fname; tell ": %v,\\n"}) fieldNames
  tell "}\""
  _ <- mapM (\fname -> do {tell ", self."; tell fname}) fieldNames
  tell ")\n"
  tell "}"

emitFile :: File -> Writer String ()
emitFile decls = do
  tell "package main\n\n"
  emitImports [("fmt", "fmt.Println"), ("math", "math.Pow")]
  intersperse (tell "\n\n") (map emit decls)

emitImports :: [(String, String)] -> Writer String ()
emitImports imps = do
  tell "import ("
  _ <- mapM (\i -> tell $ "\n\t\"" ++ i ++ "\"") $ map fst imps
  tell "\n)\n"
  _ <- mapM (\fn -> tell $ "\nvar _ = " ++ fn) $ map snd imps
  tell "\n\n"
  return ()

showFile :: File -> String
showFile = execWriter . emitFile

intersperse :: Writer String () -> [Writer String ()] -> Writer String ()
intersperse sep lst =
  case lst of
   (a1:a2:rest) -> do
     a1
     sep
     intersperse sep (a2:rest)
   [a]          -> a
   []           -> return ()

tellList :: [a] -> (a -> Writer String ()) -> Writer String ()
tellList lst f = intersperse (tell ", ") (map f lst)
