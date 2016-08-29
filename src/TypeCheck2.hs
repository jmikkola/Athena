module TypeCheck2 where

import Control.Applicative ( (<|>) )
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import IR
import qualified AST.Declaration as D
import qualified AST.Expression as E
import qualified AST.Statement as S
import qualified AST.Type as T

type Result = Either String
-- Entries in a Scope have a double-meaning:
--  - lowercase names map to the type of the value
--  - uppercae names map to the type bound to that name
type Scope = Map String Type
type TypeScope = [Scope]
type Subtypes = Map Type (Set Type)
type TSState = StateT (TypeScope, Subtypes) Result

valToTyped :: E.Value -> TSState Value
valToTyped (E.StrVal s)       = return $ StrVal s
valToTyped (E.BoolVal b)      = return $ BoolVal b
valToTyped (E.IntVal i)       = return $ IntVal i
valToTyped (E.FloatVal f)     = return $ FloatVal f
valToTyped (E.StructVal s fs) = do
  fs' <- mapM (\(name, val) -> do { val' <- exprToTyped val; return (name, val') }) fs
  return $ StructVal s fs'

exprToTyped :: E.Expression -> TSState Expression
exprToTyped e =
  case e of
   (E.Paren inner) -> do
     typedInner <- exprToTyped inner
     let innerType = typeOf typedInner
     return $ Paren typedInner innerType
   (E.Val value) -> do
     typedVal <- valToTyped value
     return $ Val typedVal
   (E.Unary op e) -> undefined -- TODO
   (E.Binary op l r) -> undefined -- TODO
   (E.Call fnEx argEs) -> undefined -- TODO
   (E.Cast t e) -> do
     innerExpr <- exprToTyped e
     return $ Cast (convertType t) innerExpr
   (E.Access e name) -> undefined -- TODO

convertType :: T.Type -> Type
convertType = undefined -- TODO
