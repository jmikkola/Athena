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

-- Monad functions --

getTypeScope :: TSState TypeScope
getTypeScope = liftM fst $ get

getSubtypes :: TSState Subtypes
getSubtypes = liftM snd $ get

putTypeScope :: TypeScope -> TSState ()
putTypeScope ts = do
  subs <- getSubtypes
  put (ts, subs)

putSubtypes :: Subtypes -> TSState ()
putSubtypes subs = do
  ts <- getTypeScope
  put (ts, subs)

updateTypeScope :: (TypeScope -> TypeScope) -> TSState ()
updateTypeScope f = do
  ts <- getTypeScope
  putTypeScope (f ts)

updateSubtypes :: (Subtypes -> Subtypes) -> TSState ()
updateSubtypes f = do
  subs <- getSubtypes
  putSubtypes (f subs)

-- Scaffolding functions --

err :: String -> TSState a
err = lift . Left

note :: a -> Maybe b -> Either a b
note msg = maybe (Left msg) Right

beginScope :: TSState ()
beginScope = updateTypeScope (Map.empty :)

endScope :: TSState ()
endScope = do
  ts <- getTypeScope
  case ts of
   []     -> err "compiler bug: ending no scopes"
   (_:ss) -> putTypeScope ss

scopeLookup :: String -> TypeScope -> Maybe Type
scopeLookup name (m:ms) = Map.lookup name m <|> scopeLookup name ms
scopeLookup _    []     = Nothing

getFromScope :: String -> TSState Type
getFromScope name = do
  ts <- getTypeScope
  lift $ note ("Not defined: " ++ name) (scopeLookup name ts)

scopeAdd :: String -> Type -> TypeScope -> Result TypeScope
scopeAdd name typ (m:ms) =
  case Map.lookup name m of
   Nothing  -> return (Map.insert name typ m : ms)
   (Just _) -> Left ("Duplicate definition for: " ++ name)
scopeAdd _    _   []     = Left "Empty scope stack??"

setInScope :: String -> Type -> TSState ()
setInScope name typ = do
  ts <- getTypeScope
  newScopes <- lift $ scopeAdd name typ ts
  putTypeScope ts

-- Typing functions --

defaultScope :: TSState ()
defaultScope = do
  setInScope "print" (Function [Named "String"] NilT)

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
   (E.Var name) -> do
     t <- getFromScope name
     return $ Var t name
   (E.Access e name) -> undefined -- TODO

convertType :: T.Type -> Type
convertType t = case t of
  T.String           -> Named "String"
  T.Float            -> Named "Float"
  T.Int              -> Named "Int"
  T.Bool             -> Named "Bool"
  T.Nil              -> NilT
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
