module Emit where

import Control.Monad.Writer

import AST.Expression (Expression, Value, UnaryOp, BinOp)
import qualified AST.Expression as E
import AST.Type (Type)
import qualified AST.Type as T

class Render a where
  render :: a -> String

class Emitter a where
  emit :: a -> Writer String ()

instance Render Value where
  render v = case v of
    (E.EString s) -> show s
    (E.EBool b)   -> if b then "true" else "false"
    (E.EInt i)    -> show i
    (E.EFloat f)  -> show f

instance Emitter Value where
  emit = tell . render

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
    emit l
    tell " "
    emit op
    tell " "
    emit r
  emit (E.ECall fex argex) = do
    emit fex
    tell "("
    emitArgs argex
    tell ")"
  emit (E.ECast t e)       = do
    emit t
    tell "("
    emit e
    tell ")"
  emit (E.EVariable v)     =
    tell v

emitArgs :: (Emitter a) => [a] -> Writer String ()
emitArgs (a1:a2:rest) = do
  emit a1
  tell ", "
  emitArgs (a2:rest)
emitArgs [a] = emit a
emitArgs [] = return ()

instance Render UnaryOp where
  render E.BitInvert = "~"
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
  render E.BitXor    = "^"
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
    emitArgs args
    tell ")"
    if ret /= T.Nil then tell " " else return ()
    emit ret
