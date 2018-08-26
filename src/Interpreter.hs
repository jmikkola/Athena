{-# LANGUAGE DeriveFunctor #-}

module Interpreter (interpret) where


import System.IO (hFlush, stdout)

import Control.Applicative ( (<|>) )
import Control.Monad.Free (Free (..))

import Data.Map (Map)
import qualified Data.Map as Map

import Inference
  ( InferResult(..) )
import qualified AST.Expression as E
import qualified AST.Statement as S
import Types (Type(..), tUnit)


interpret :: InferResult () -> IO ()
interpret body =
  let mainT = TFunc [] tUnit
      callMain = E.Call (tUnit, ()) (E.Var (mainT, ()) "main") []
  in do
    _ <- runOp (interpretExpr body callMain) startingState
    return ()


startingState :: Scope
startingState = [Map.empty]

runOp :: Free IOOp Value -> Scope -> IO Value
runOp = undefined

interpretExpr :: InferResult () -> E.Expression AnnT -> Free IOOp Value
interpretExpr = undefined


data IOOp next
  = ExitSuccess
  | ExitError
  | ReadLine (String -> next)
  | WriteLine String next
  | GetVar String (Value -> next)
  | PutVar String Value next
  | Error String
  deriving (Functor)

data Value
  = VInt Int
  | VFloat Float
  | VString String
  | VBool Bool
  | VList [Value]
  | VClosure Value Function
  | VVoid
  deriving (Show)

data Function
  = Function [String] (S.Statement AnnT)
  deriving (Show)

type Scope = [Map String Value]

-- AnnT is short for "Annotation Type"
type AnnT = (Type, ())
