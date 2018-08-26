module Interpreter (interpret) where


import System.IO (hFlush, stdout)

import Data.Map (Map)
import qualified Data.Map as Map

import Inference
  ( InferResult(..)
  , topLevelBindings )
import qualified AST.Declaration as D
import qualified AST.Expression as E
import qualified AST.Statement as S
import Types (Type(..), tUnit)


interpret :: InferResult () -> IO ()
interpret body =
  let mainT = TFunc [] tUnit
      callMain = E.Call (tUnit, ()) (E.Var (mainT, ()) "main") []
  in do
    let scope = startingState body
    _ <- interpretExpr scope callMain
    return ()


startingState :: InferResult () -> Scope
startingState body =
  let decls = topLevelBindings body
      -- TODO: Also evaluate constant expression
      bindings =
        [ (name, toClosure scope args stmt)
        | (name, D.Function _ _ _ args stmt) <- decls ]
      scope = [Map.fromList bindings]
  in scope

interpretExpr :: Scope -> E.Expression AnnT -> IO Value
interpretExpr scope expr = case expr of
  E.Paren _ ex ->
    interpretExpr scope ex
  E.Val _ val ->
    convertValue val
  E.Unary _ uop ex -> do
    val <- interpretExpr scope ex
    applyUOp uop val
  E.Binary _ bop l r -> do
    lVal <- interpretExpr scope l
    rVal <- interpretExpr scope r
    applyBOp bop lVal rVal
  E.Call _ fnEx argExs -> do
    fnVal <- interpretExpr scope fnEx
    argVals <- mapM (interpretExpr scope) argExs
    callFunction scope fnVal argVals
  E.Cast _ t ex -> do
    val <- interpretExpr scope ex
    castVal t val
  E.Var _ name -> do
    lookupVar scope name
  E.Access _ ex field -> do
    val <- interpretExpr scope ex
    accessField val field


convertValue :: E.Value a -> IO Value
convertValue = undefined


applyUOp :: E.UnaryOp -> Value -> IO Value
applyUOp = undefined

applyBOp :: E.BinOp -> Value -> Value -> IO Value
applyBOp = undefined

callFunction :: Scope -> Value -> [Value] -> IO Value
callFunction = undefined

castVal :: String -> Value -> IO Value
castVal t val = undefined

lookupVar :: Scope -> String -> IO Value
lookupVar = undefined

accessField :: Value -> String -> IO Value
accessField = undefined

data Value
  = VInt Int
  | VFloat Float
  | VString String
  | VBool Bool
  | VList [Value]
  | VClosure Scope Function
  | VVoid
  deriving (Show)

data Function
  = Function [String] (S.Statement AnnT)
  deriving (Show)

type Scope = [Map String Value]

-- AnnT is short for "Annotation Type"
type AnnT = (Type, ())


toClosure :: Scope -> [String] -> S.Statement AnnT -> Value
toClosure scope args stmt =
  VClosure scope $ Function args stmt
