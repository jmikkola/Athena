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
  fs' <- mapMSnd exprToTyped fs
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
   (E.Var var) -> undefined -- TODO
   (E.Access e name) -> undefined -- TODO

convertType :: T.Type -> Type
convertType t = case t of
  T.String           -> Named "String"
  T.Float            -> Named "Float"
  T.Int              -> Named "Int"
  T.Bool             -> Named "Bool"
  T.Nil              -> Named "()"
  (T.TypeName n)     -> Named n
  (T.Function at rt) -> Function (map convertType at) (convertType rt)
  (T.Struct fields)  -> Struct $ mapSnd convertType fields
  (T.Enum options)   -> Enum $ mapSnd (mapSnd convertType) options

mapSnd :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnd f = map (\(c, a) -> (c, f a))

mapMSnd f = mapM f'
  where f' (c, a) = do
          b <- f a
          return (c, b)
