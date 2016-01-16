module InferExpression where

{-
TODO:
- Store a mapping of type constructor names to types
-}

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
                       }
             deriving (Show)

startingState :: TIState
startingState = TIState emptyRules [1..] emptyScope emptyRegistry

register :: (Typeable a) => a -> VarGen -> Registry -> (TypeVar, VarGen, Registry)
register a vargen reg =
  let (tvar:vargen') = vargen
  in (tvar, vargen', Map.insert (asEntry a) tvar reg)

lookupVar :: String -> TIState -> ErrorS (TypeVar)
lookupVar varName (TIState _ _ scope _) =
  case Map.lookup varName (current scope) of
    Nothing -> Left $ "Variable " ++ varName ++ " not found in scope " ++ (show scope)
    Just tv -> Right tv

addRule :: (Rules -> Rules) -> TIState -> TIState
addRule rule tistate = tistate { tirules = rule (tirules tistate) }

boolTN :: TypeNode
boolTN = TypeNode { constructor = "Bool", components = [] }

class Typeable a where
  asEntry :: a -> TypeEntry
  ti :: TIState -> a -> ErrorS (TIState, TypeVar)

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
     (ExpressionStruct tname exprs)      -> undefined
     (ExpressionIf test ifcase elsecase) -> do
       (tistate1, ttv) <- ti tistate test
       (tistate2, itv) <- ti tistate1 ifcase
       (tistate3, etv) <- ti tistate2 elsecase
       let tistate4 = addRule (specify ttv boolTN) $ addRule (setEqual itv etv) tistate3
       return (tistate4, itv)
