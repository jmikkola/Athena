module InferExpression where

import Data.Map (Map)
import qualified Data.Map as Map

import Parse
  ( LiteralValue (..)
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
               deriving (Show, Eq, Ord)

type Registry = Map TypeEntry TypeVar

emptyRegistry :: Registry
emptyRegistry = Map.empty

data TIState = TIState Rules VarGen Scopes Registry
             deriving (Show)

startingState :: TIState
startingState = TIState emptyRules [1..] emptyScope emptyRegistry

class Typeable a where
  asEntry :: a -> TypeEntry
  ti :: TIState -> a -> ErrorS (TIState, TypeVar)

register :: (Typeable a) => a -> VarGen -> Registry -> (TypeVar, VarGen, Registry)
register a vargen reg =
  let (tvar:vargen') = vargen
  in (tvar, vargen', Map.insert (asEntry a) tvar reg)

instance Typeable LiteralValue where
  asEntry = LitEntry

  ti (TIState rules vargen scopes reg) lit =
    let typeName = case lit of
          LiteralFloat _  -> "Float"
          LiteralInt _    -> "Int"
          LiteralString _ -> "String"
          LiteralChar _   -> "Char"
        typeNode = TypeNode { constructor = typeName, components = [] }
        (tvar, vargen', reg') = register lit vargen reg
        rules' = specify tvar typeNode rules
    in return (TIState rules' vargen' scopes reg', tvar)
