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
  , TypeDef (..)
  , Block (..)
  , ElsePart
  , MatchPattern
  , display
  , fnArgNames
  , fnName
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

boolTN :: TypeNode
boolTN = TypeNode { constructor = "Bool", components = [] }

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

addToScope :: Scopes -> Scope -> Scopes
addToScope (Scopes cur pts) scope =
  let cur' = Map.union scope cur
  in Scopes cur' pts

popScope :: Scopes -> Scopes
popScope (Scopes _ [])     = error "Bug: trying to pop scope too many times"
popScope (Scopes _ (s:ss)) = Scopes s ss

scopeLookup :: String -> Scopes -> Maybe ScopedVar
scopeLookup name scopes = Map.lookup name (current scopes)

-- TE = Typeable Expression
data TE = TETyped Type TE
        | TEVar String
        | TELit Type LiteralValue
          -- function expr, arg exprs
        | TEAp TE [TE]
          -- bindings, body
        | TELet [(String, TE)] TE
          -- var name (existing), value
        | TESet String TE
          -- sequence two expressions, evalutate to the last
        | TESeq TE TE
          -- doesn't evaluate to anything:
        | TENil
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

defaultCtors :: Map String String
defaultCtors = Map.fromList
               [ ("True", "Bool")
               , ("False", "Bool")
               , ("List", "List")
               ]

nilTypeNode :: TypeNode
nilTypeNode = TypeNode "()" []

startingState :: TIState
startingState = TIState emptyRules [1..] emptyScope emptyRegistry defaultCtors

createScope :: TIState -> [(String, ScopedVar)] -> TIState
createScope tistate scopedVars =
  let scopes = pushScope (tiscopes tistate)
      scopes' = addToScope scopes (Map.fromList scopedVars)
  in tistate { tiscopes=scopes' }

endScope :: TIState -> TIState
endScope tistate =
  let scopes = popScope (tiscopes tistate)
  in tistate { tiscopes=scopes }

nextTV :: TIState -> (TIState, TypeVar)
nextTV tistate =
  let (tvar:vargen) = tivargen tistate
  in (tistate { tivargen=vargen }, tvar)

nextTVs :: TIState -> Int -> (TIState, [TypeVar])
nextTVs tistate n =
  let vargen = tivargen tistate
      vars = take n vargen
      vargen' = drop n vargen
  in (tistate { tivargen=vargen' }, vars)

addRule :: TIState -> (Rules -> Rules) -> TIState
addRule tistate rule =
  let rules' = rule (tirules tistate)
  in tistate { tirules=rules' }

addRules :: TIState -> [Rules -> Rules] -> TIState
addRules tistate rules =
  let rules' = foldl (\rs r -> r rs) (tirules tistate) rules
  in tistate { tirules=rules' }

register :: TIState -> TE -> TypeVar -> TIState
register tistate te tvar =
  let reg' = Map.insert te tvar (tireg tistate)
  in tistate { tireg=reg' }

lookupTE :: TIState -> TE -> Maybe TypeVar
lookupTE tistate te = Map.lookup te (tireg tistate)

-- this is mostly just here to make lookupTE useable within the Monad
requireTEVar :: TIState -> TE -> ErrorS TypeVar
requireTEVar tistate te = case lookupTE tistate te of
  Nothing -> Left $ "Could not find type var for: " ++ show te
  Just tv -> Right tv

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

class ToTE a where
  toTE :: a -> ErrorS TE

instance ToTE LiteralValue where
  toTE l = return $ TELit tp l
    where tp = case l of
                 (LiteralFloat _)  -> Constructor "Float" []
                 (LiteralInt _)    -> Constructor "Int" []
                 (LiteralString _) -> Constructor "String" []
                 (LiteralChar _)   -> Constructor "Char" []

instance ToTE Expression where
  toTE (ExpressionLit l)               = toTE l
  toTE (ExpressionVar v)               = return $ TEVar v
  toTE (ExpressionParen e)             = toTE e
  -- TODO: change ExpressionFnCall to accept an expression as the function
  toTE (ExpressionFnCall fnname exprs) = do
    exprTEs <- mapM toTE exprs
    return $ TEAp (TEVar fnname) exprTEs
  toTE (ExpressionBinary op l r)       = do
    exprTEs <- mapM toTE [l, r]
    return $ TEAp (TEVar $ display op) exprTEs
  toTE (ExpressionUnary op e)          = do
    exprTE <- toTE e
    return $ TEAp (TEVar $ display op) [exprTE]
  -- TDDO: This assumes that struct definitions register a function for each constructor
  toTE (ExpressionStruct name exprs)   = do
    exprTEs <- mapM toTE exprs
    return $ TEAp (TEVar name) exprTEs
  toTE (ExpressionIf test ifp elsep)   = do
    testTE <- toTE test
    ifpTE <- toTE ifp
    elsepTE <- toTE elsep
    return $ TEIf testTE ifpTE elsepTE

data TopLevel = TopLevel [Statement]

instance ToTE TopLevel where
  toTE (TopLevel stmts) = do
    letBindings <- foldM gatherBindings Map.empty stmts
    let body = undefined -- well fuck, what is the body?
    return $ TELet (Map.toList letBindings) body

gatherBindings :: Map String TE -> Statement -> ErrorS (Map String TE)
gatherBindings existing stmt = case stmt of
  (StatementLet varName expr) -> do
    te <- toTE expr
    addBinding existing varName te
  (StatementFn funcDef)      -> do
    let name = fnName funcDef
    te <- toTE funcDef
    addBinding existing name te
  _                          -> Left ("Statement not allowed at top level: " ++ show stmt)

addBinding :: Map String TE -> String -> TE -> ErrorS (Map String TE)
addBinding existing name te = case Map.lookup name existing of
  Nothing  -> return $ Map.insert name te existing
  (Just x) -> Left ("Duplicate binding for " ++ name ++ ": " ++ show x ++ " and " ++ show te)

instance ToTE FunctionDef where
  -- TODO: read type expressions
  toTE f = do
    bodyExpr <- case f of
      (FunctionDef _ _ _ blk) -> toTE blk
      (ShortFn _ _ _ expr)    -> toTE expr
    return $ TELam (fnArgNames f) bodyExpr

instance ToTE Block where
  toTE (Block stmts) = buildFnBody stmts

buildFnBody :: [Statement] -> ErrorS TE
buildFnBody []          = return TENil
buildFnBody (stmt:rest) = case stmt of
  (StatementReturn expr)     ->
    if null rest
    then toTE expr
    else Left ("Statements after a return: " ++ show rest)
  (StatementExpr expr)         -> do
    exprTE <- toTE expr
    restTE <- buildFnBody rest
    return $ TESeq exprTE restTE
  (StatementLet var expr)      -> do
    exprTE <- toTE expr
    restTE <- buildFnBody rest
    return $ TELet [(var, exprTE)] restTE
  (StatementAssign var expr)   -> do
    exprTE <- toTE expr
    restTE <- buildFnBody rest
    return $ TESeq (TESet var exprTE) restTE
  (StatementIf test blk ep)   -> do
    restTE <- buildFnBody rest
    return undefined -- TODO: what if there is a return inside the block?
  (StatementWhile test blk)   -> do
    restTE <- buildFnBody rest
    return undefined -- TODO: what if there is a return inside the block?
  (StatementFor var test blk) -> do
    restTE <- buildFnBody rest
    return undefined -- TODO: what if there is a return inside the block?
  (StatementFn funcDef)       -> do
    funcBodyTE <- toTE funcDef
    restTE <- buildFnBody rest
    return $ TELet [(fnName funcDef, funcBodyTE)] restTE

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

  (TELit tp _)       ->
    let (tistate1, tv) = nextTV tistate
        tistate2 = register tistate1 te tv
        tistate3 = specifyType tv tp tistate2
    in return (tv, tistate3)

  (TEAp fnexp args)  -> do
    let (tistate1, tv) = nextTV tistate
    (fnTV, tistate2) <- gatherRules tistate1 fnexp
    (argTVs, tistate3) <- gatherRuleList tistate2 args
    let fnName = makeFnName (length args)
    let fnType = TypeNode { constructor=fnName, components=(argTVs ++ [tv]) }
    let tistate4 = addRule tistate3 (specify fnTV fnType)
    return (tv, tistate4)

  (TELet binds body) -> do
    let (bindNames, bindExprs) = unzip binds
    let (tistate1, tv) = nextTV tistate
    -- Create a new scope with the bindings added
    -- (the new bindings are mapped to new type variables)
    let (tistate2, bindTVs) = nextTVs tistate1 (length binds)
    let newScope = zipWith (\name tv -> (name, ScopedVar tv True)) bindNames bindTVs
    let tistate3 = createScope tistate2 newScope
    -- Gather the rules under that scope
    (boundTVs, tistate4) <- gatherRuleList tistate3 bindExprs
    (bodyTV, tistate5) <- gatherRules tistate4 body
    -- Set those variables equal to the real things
    let tistate6 = addRule tistate5 (setEqual bodyTV tv)
    let tistate7 = addRules tistate6 (zipWith setEqual bindTVs boundTVs)
    -- Close the scope
    let tistate8 = endScope tistate7
    return (tv, tistate8)

  (TELam args body)  -> do
    let (tistate1, tv) = nextTV tistate
    let tistate2 = register tistate1 te tv
    let (tistate3, argTVs) = nextTVs tistate2 (length args)
    -- False because this doesn't support 2nd order polymorphism
    let newScope = zipWith (\name tv -> (name, ScopedVar tv False)) args argTVs
    let tistate4 = createScope tistate3 newScope
    (bodyTV, tistate5) <- gatherRules tistate4 body
    let fnName = makeFnName (length args)
    let thisType = TypeNode { constructor=fnName, components=(argTVs ++ [bodyTV]) }
    let tistate6 = addRule tistate5 (specify tv thisType)
    let tistate7 = endScope tistate6
    return (tv, tistate7)

  (TEIf test ifCase elseCase) -> do
    let (tistate1, tv) = nextTV tistate
    let tistate2 = register tistate1 te tv
    (testTV, tistate3) <- gatherRules tistate2 test
    (ifTV, tistate4) <- gatherRules tistate3 ifCase
    (elseTV, tistate5) <- gatherRules tistate4 elseCase
    let tistate6 = addRules tistate5 [ specify testTV boolTN
                                     , setEqual tv ifTV
                                     , setEqual tv elseTV ]
    return (tv, tistate6)

  TENil                       -> do
    let (tistate1, tv) = nextTV tistate
    return (tv, addRule tistate1 (specify tv nilTypeNode))
  (TESeq start result)        -> do
    (_, tistate1) <- gatherRules tistate start
    gatherRules tistate1 result
  (TESet name value)          -> do
    var <- lookupVar name tistate
    (valueTV, tistate1) <- gatherRules tistate value
    let tistate2 = addRule tistate1 (setEqual valueTV (scTypeVar var))
    -- Assume that the parser will never allow an assignment to be used in an expression:
    return (valueTV, tistate2)

makeFnName :: Int -> String
makeFnName numArgs = "Fn_" ++ show numArgs

gatherRuleList :: TIState -> [TE] -> ErrorS ([TypeVar], TIState)
gatherRuleList tistate exprs = grl exprs tistate []
  where grl []       st vars = return (reverse vars, st)
        grl (te:tes) st vars = do
          (tv, tistate) <- gatherRules st te
          grl tes tistate (tv : vars)
