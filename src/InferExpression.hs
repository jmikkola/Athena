module InferExpression where

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map

{-
TODO: Add kind-checking where appropriate
-}

import Parse
  ( LiteralValue (..)
  , Expression (..)
  , BinaryOp
  , UnaryOp
  , Statement (..)
  , FunctionDef (..)
  , FnArg
  , TypeDef
  , Block (..)
  , ElsePart
  , MatchPattern
  , display
  )
import TypeInference
  ( Rules
  , ErrorS
  , VarGen
  , TypeNode (..)
  , TypeVar
  , Type (..)
  , emptyRules
  , setEqual
  , specify
  , instanceOf
  )

data ScopedVar = ScopedVar { scTypeVar   :: TypeVar
                           , scIsGeneric :: Bool
                           }
                 deriving (Show, Eq, Ord)
type Scope = Map String ScopedVar
data Scopes = Scopes { current :: Scope
                     , parents :: [Scope]
                     }
            deriving (Show)

emptyScope :: Scopes
emptyScope = Scopes { current = Map.empty, parents = [] }

pushScope :: Scopes -> Scopes
pushScope (Scopes cur pts) = Scopes { current = cur, parents = cur:pts }


-- TE = Typeable Expression
data TE = TETyped Type TE
        | TEVar String
        | TELit Type LiteralValue
          -- function expr, arg exprs
        | TEAp TE [TE]
          -- bindings, body
        | TELet [(String, TE)] TE
          -- arg names, body
        | TELam [String] TE
          -- test, if case, else case
        | TEIf TE TE TE
        deriving (Eq, Ord, Show)

type Registry = Map TE TypeVar

emptyRegistry :: Registry
emptyRegistry = Map.empty

data TIState = TIState { tirules  :: Rules
                       , tivargen :: VarGen
                       , tiscopes :: Scopes
                       , tireg    :: Registry
                       , tictors  :: Map String String
                       }
             deriving (Show)

defaultCtors = Map.fromList
               [ ("True", "Bool")
               , ("False", "Bool")
               , ("List", "List")
               ]

startingState :: TIState
startingState = TIState emptyRules [1..] emptyScope emptyRegistry defaultCtors

nextTV :: TIState -> (TIState, TypeVar)
nextTV tistate =
  let (tvar:vargen) = tivargen tistate
  in (tistate { tivargen=vargen }, tvar)

addRule :: TIState -> (Rules -> Rules) -> TIState
addRule tistate rule =
  let rules' = rule (tirules tistate)
  in tistate { tirules=rules' }

register :: TIState -> TE -> TypeVar -> TIState
register tistate te tvar =
  let reg' = Map.insert te tvar (tireg tistate)
  in tistate { tireg=reg' }

lookupVar :: String -> TIState -> ErrorS (ScopedVar)
lookupVar varName tistate =
  case Map.lookup varName (current $ tiscopes tistate) of
    Nothing -> Left $ "Variable " ++ varName ++ " not found in scope " ++ (show $ tiscopes tistate)
    Just sv -> Right sv

buildTns :: TIState -> [Type] -> (TIState, [TypeVar])
buildTns tistate subtypes = build_ subtypes tistate []
  where build_ []     st vars = (st, reverse vars)
        build_ (t:ts) st vars =
          let (tistate', tv) = buildTn st t
          in build_ ts tistate' (tv:vars)

buildTn :: TIState -> Type -> (TIState, TypeVar)
buildTn tistate (Var tv) = (tistate, tv)
buildTn tistate (Constructor name subtypes) =
  let (tistate1, subtypeVars) = buildTns tistate subtypes
      (tistate2, tv) = nextTV tistate1
      typeNode = TypeNode { constructor=name, components=subtypeVars }
      tistate3 = addRule tistate2 (specify tv typeNode)
  in (tistate3, tv)

specifyType :: TypeVar -> Type -> TIState -> TIState
specifyType tv tp tistate =
  let (tistate', tpVar) = buildTn tistate tp
      rules = tirules tistate'
      rules' = setEqual tv tpVar rules
  in tistate' { tirules = rules' }

gatherRules :: TIState -> TE -> ErrorS (TypeVar, TIState)
gatherRules tistate te = case te of
  (TETyped tp inner) -> do
    (tv, tistate1) <- gatherRules tistate inner
    let tistate2 = specifyType tv tp tistate1
    let tistate3 = register tistate2 te tv
    return (tv, tistate3)
  (TEVar name)       -> do
    var <- lookupVar name tistate
    let tvar = scTypeVar var
    if scIsGeneric var
      then let (tistate1, gtv) = nextTV tistate
               tistate2 = register tistate1 te gtv
               -- here's where generics happen
               tistate3 = addRule tistate2 (instanceOf gtv tvar)
           in return (gtv, tistate3)
      else let tistate1 = register tistate te tvar
           in return (tvar, tistate1)
  _ -> undefined
{-

data TypeEntry = LitEntry LiteralValue
               | ExprEntry Expression
               | StmtEntry Statement
               | BlockEntry Block
               | FnEntry FunctionDef
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

lookupVar :: String -> TIState -> ErrorS (ScopedVar)
lookupVar varName tistate =
  case Map.lookup varName (current $ tiscopes tistate) of
    Nothing -> Left $ "Variable " ++ varName ++ " not found in scope " ++ (show $ tiscopes tistate)
    Just sv -> Right sv

lookupCtor :: String -> TIState -> ErrorS (String)
lookupCtor cfn tistate =
  case Map.lookup cfn (tictors tistate) of
    Nothing -> Left $ "Constructor " ++ cfn ++ " not found"
    Just tp -> Right tp

addRule :: (Rules -> Rules) -> TIState -> TIState
addRule rule tistate = tistate { tirules = rule (tirules tistate) }

addRules :: [Rules -> Rules] -> TIState -> TIState
addRules rules tistate = foldl (flip addRule) tistate rules

boolTN :: TypeNode
boolTN = TypeNode { constructor = "Bool", components = [] }

nullTN :: TypeNode
nullTN = TypeNode { constructor = "()", components = [] }

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
       scopedVar <- lookupVar varName tistate
       let varTV = scTypeVar scopedVar
       let (tv, tistate1) = register expr tistate
       let rule = if scIsGeneric scopedVar
                  then setEqual varTV tv
                  else instanceOf varTV tv
       return (addRule rule tistate1, varTV)
     (ExpressionParen expr)              -> ti tistate expr
     (ExpressionFnCall fname exprs)      -> tiFnCall tistate expr fname exprs
     (ExpressionBinary op left right)    -> tiFnCall tistate expr (display op) [left, right]
     (ExpressionUnary op expr)           -> tiFnCall tistate expr (display op) [expr]
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

instance Typeable Statement where
  asEntry = StmtEntry

  ti tistate stmt =
    case stmt of
     (StatementExpr expr) -> ti tistate expr
     (StatementLet varName expr) -> tiLet tistate varName expr
     (StatementAssign varName expr) -> undefined
     (StatementReturn expr) -> ti tistate expr
     (StatementIf test ifblk elsepart) -> undefined
     (StatementWhile test blk) -> undefined
     (StatementFor varName listExpr blk) -> undefined
     (StatementMatch expr cases) -> undefined
     (StatementFn fnDef) -> undefined

instance Typeable Block where
  asEntry = BlockEntry

  ti tistate blk@(Block []) = do
    -- Special case: avoid blowing up for empty blocks by returning the `()` type
    let (tv, tistate') = register blk tistate
    return (addRule (specify tv nullTN) tistate', tv)
  ti tistate (Block stmts)  = do
    (tistate', tvs) <- tiList tistate stmts
    -- If a block's type is used, it's always the last type
    return (tistate', last tvs)

instance Typeable FunctionDef where
  asEntry = FnEntry

  ti tistate fndef = do
    let (tv, tistate1) = register fndef tistate
    -- TODO: add function to scope here
    (tistate2, fnname, args, tdef, bodyType) <- case fndef of
          (FunctionDef name args tdef block) -> do
            -- TODO: add args to scope here
            (tistate', blockTv) <- ti tistate1 block
            return (tistate', name, args, tdef, blockTv)
          (ShortFn name args tdef expr) -> do
            -- TODO: add args to scope here
            (tistate', exprTv) <- ti tistate1 expr
            return (tistate', name, args, tdef, exprTv)
    return (tistate2, tv)

tiList :: (Typeable a) => TIState -> [a] -> ErrorS (TIState, [TypeVar])
-- now here is where having `fmap` would be nice...
tiList st xs = recur st xs []
  where recur tist []     rest = Right (tist, reverse rest)
        recur tist (x:xs) rest = do
          (tist', xtv) <- ti tist x
          recur tist' xs (xtv:rest)

tiFnCall :: TIState -> Expression -> String -> [Expression] -> ErrorS (TIState, TypeVar)
tiFnCall tistate fncall fnname args = do
  (tistate1, fnTypeVar) <- ti tistate (ExpressionVar fnname)
  let (tv, tistate2) = register fncall tistate
  (tistate3, argTypes) <- tiList tistate2 args
  let (retTV, tistate4) = nextTV tistate3
  let fnTypeName = "Fn<" ++ show (length args) ++ ">"
  let fnTypeNode = TypeNode { constructor=fnTypeName, components=(argTypes ++ [retTV]) }
  let rules = [setEqual tv retTV, specify fnTypeVar fnTypeNode]
  return (addRules rules tistate4, tv)

tiLet :: TIState -> String -> Expression -> ErrorS (TIState, TypeVar)
tiLet = undefined
-}
