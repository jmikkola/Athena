module InferExpression where

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map

import Parse
  ( LiteralValue (..)
  , Expression (..)
  )
import TypeInference
  ( Rules
  , ErrorS
  , VarGen
  , TypeNode (..)
  , TypeVar
  , emptyRules
  , setEqual
  , specify
  , instanceOf
  )

type Scope = Map String TypeVar
data Scopes = Scopes { current :: Scope
                     , parents :: [Scope]
                     }
            deriving (Show)

emptyScope :: Scopes
emptyScope = Scopes { current = Map.empty, parents = [] }

pushScope :: Scopes -> Scopes
pushScope (Scopes cur pts) = Scopes { current = cur, parents = cur:pts }

data TypeEntry = LitEntry LiteralValue
               | ExprEntry Expression
               deriving (Show, Eq, Ord)

type Registry = Map TypeEntry TypeVar

emptyRegistry :: Registry
emptyRegistry = Map.empty

data TIState = TIState { tirules  :: Rules
                       , tivargen :: VarGen
                       , tiscopes :: Scopes
                       , tireg    :: Registry
                       , tictors  :: Map String String
                       }
             deriving (Show)

-- TODO: also store the number of fields each type has
-- TODO: figure out how to deal with generic types
defaultCtors = Map.fromList
               [ ("True", "Bool")
               , ("False", "Bool")
               , ("List", "List")
               ]

startingState :: TIState
startingState = TIState emptyRules [1..] emptyScope emptyRegistry defaultCtors

register :: (Typeable a) => a -> TIState -> (TypeVar, TIState)
register a tistate =
  let (tvar:vargen) = tivargen tistate
      registry = Map.insert (asEntry a) tvar (tireg tistate)
  in (tvar, tistate { tivargen=vargen, tireg=registry })

lookupVar :: String -> TIState -> ErrorS (TypeVar)
lookupVar varName tistate =
  case Map.lookup varName (current $ tiscopes tistate) of
    Nothing -> Left $ "Variable " ++ varName ++ " not found in scope " ++ (show $ tiscopes tistate)
    Just tv -> Right tv

lookupCtor :: String -> TIState -> ErrorS (String)
lookupCtor cfn tistate =
  case Map.lookup cfn (tictors tistate) of
    Nothing -> Left $ "Constructor " ++ cfn ++ " not found"
    Just tp -> Right tp

addRule :: (Rules -> Rules) -> TIState -> TIState
addRule rule tistate = tistate { tirules = rule (tirules tistate) }

boolTN :: TypeNode
boolTN = TypeNode { constructor = "Bool", components = [] }

class Typeable a where
  asEntry :: a -> TypeEntry
  ti :: TIState -> a -> ErrorS (TIState, TypeVar)

instance Typeable LiteralValue where
  asEntry = LitEntry

  ti tistate lit =
    let typeName = case lit of
          LiteralFloat _  -> "Float"
          LiteralInt _    -> "Int"
          LiteralString _ -> "String"
          LiteralChar _   -> "Char"
        typeNode = TypeNode { constructor = typeName, components = [] }
        (tvar, tistate') = register lit tistate
        tistate'' = addRule (specify tvar typeNode) tistate'
    in return (tistate'', tvar)

instance Typeable Expression where
  asEntry = ExprEntry

  ti tistate expr =
    case expr of
     (ExpressionLit lit)                 -> ti tistate lit
     (ExpressionVar varName)             -> do
       tv <- lookupVar varName tistate
       return (tistate, tv)
     (ExpressionParen expr)              -> ti tistate expr
     (ExpressionFnCall fname exprs)      -> undefined
     (ExpressionBinary op left right)    -> undefined
     (ExpressionUnary op expr)           -> undefined
     (ExpressionStruct tname exprs)      -> do
       -- TODO: this might not correctly handle generic instantiation
       let (tv, tistate1) = register expr tistate
       structType <- lookupCtor tname tistate1
       (tistate2, exprTypes) <- tiList tistate1 exprs
       let sttype = TypeNode { constructor=structType, components=exprTypes }
       let tistate3 = addRule (specify tv sttype) tistate2
       return (tistate3, tv)
     (ExpressionIf test ifcase elsecase) -> do
       (tistate1, ttv) <- ti tistate test
       (tistate2, itv) <- ti tistate1 ifcase
       (tistate3, etv) <- ti tistate2 elsecase
       let tistate4 = addRule (specify ttv boolTN) $ addRule (setEqual itv etv) tistate3
       return (tistate4, itv)

tiList :: (Typeable a) => TIState -> [a] -> ErrorS (TIState, [TypeVar])
-- now here is where having `fmap` would be nice...
tiList st xs = recur st xs []
  where recur tist []     rest = Right (tist, reverse rest)
        recur tist (x:xs) rest = do
          (tist', xtv) <- ti tist x
          recur tist' xs (xtv:rest)
