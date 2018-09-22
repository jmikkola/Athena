module Inference
  ( inferModule
  , mgu
  , startingEnv
  , runInfer
  , inferExpr
  , inferDecl
  , unifies
  , makeBindGroups
  , implicitBindings
  , instantiate
  , BindGroup
  , Environment
  , TypedDecls
  , InferResult(..)
  ) where


import Prelude hiding (exp)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

--import Debug.Trace

import Control.Monad.State (StateT, modify, get, put, lift, evalStateT)


import AST.Annotation (Annotated, getAnnotation)
import AST.Expression (UnaryOp(..), BinOp(..))
import qualified AST.Expression as E
import qualified AST.Statement as S
import qualified AST.Declaration as D
--import qualified AST.Type as T

import Types
  ( Substitution
  , Scheme(..)
  , Type(..)
  , tInt
  , tFloat
  , tBool
  , tString
  , tUnit
  , apply
  , asScheme
  , composeSubs
  , emptySubstitution
  , freeTypeVars )
import Errors
  ( Error(..)
  , Result )
import FirstPass
  ( Module(..), bindings )
import Util.Graph
  ( components )


newtype BindGroup a
  = BindGroup
    { implicitBindings :: [(String, D.Declaration a)] }
    -- TODO: Add explicit bindings

type TypedDecls a = [(String, D.Declaration (Type, a))]

data InferResult a
  = InferResult
    { topLevelBindings :: TypedDecls a
    , topLevelEnv      :: Environment }
  deriving (Show)

-- (Type, a) adds a type to whatever the original annotation was
inferModule :: Module a -> Result (InferResult a)
inferModule m = do
  let bindGroups = makeBindGroups m
  (binds, env) <- runInfer $ inferGroups bindGroups startingEnv
  return $ InferResult { topLevelBindings=binds, topLevelEnv=env }

makeBindGroups :: Module a -> [BindGroup a]
makeBindGroups m =
  let declarations = bindings m
      graph = gatherGraph declarations
      topoOrder = reverse $ components graph
      getBinding name = (name, mustLookup name declarations)
  in map BindGroup $ map (map getBinding) topoOrder

-- TODO: extend this into prelude (plus imported names)
startingDependencies :: Set String
startingDependencies = Set.fromList ["print"]

-- This walks each declaration to find out what the
-- dependency graph looks like.
-- This assumes that all the variables are defined (TODO: that's
-- never checked at the moment)
gatherGraph :: Map String (D.Declaration a) -> Map String [String]
gatherGraph decls = Map.map (unique . findDependencies startingDependencies) decls

unique :: (Ord a) => [a] -> [a]
unique = Set.toList . Set.fromList

class Depencencies a where
  findDependencies :: Set String -> a -> [String]

instance Depencencies (D.Declaration a) where
  findDependencies bound decl = case decl of
    D.Let _ name exp ->
      findDependencies (Set.insert name bound) exp
    D.Function _ name args stmt ->
      findDependencies (Set.union bound $ Set.fromList (name:args)) stmt
    D.TypeDef _ _ _ ->
      []

instance Depencencies (S.Statement a) where
  findDependencies bound stmt = case stmt of
    S.Return _ mexp ->
      fromMaybe [] $ fmap (findDependencies bound) mexp
    S.Let _ name exp ->
      findDependencies (Set.insert name bound) exp
    S.Assign _ _ exp ->
      findDependencies bound exp
    S.Block _ stmts ->
      findDepBlock bound stmts
    S.Expr _ expr ->
      findDependencies bound expr
    S.If _ test body elseStmt ->
      let testDeps = findDependencies bound test
          bodyDeps = findDepBlock bound body
          elseDeps = fromMaybe [] $ fmap (findDependencies bound) elseStmt
      in testDeps ++ bodyDeps ++ elseDeps
    S.While _ test body ->
      let testDeps = findDependencies bound test
          bodyDeps = findDepBlock bound body
      in testDeps ++ bodyDeps

findDepBlock :: Set String -> [S.Statement a] -> [String]
findDepBlock bound stmts = case stmts of
     [] -> []
     (stmt:rest) ->
       let stmtDeps = findDependencies bound stmt
           bound' = case stmt of
             (S.Let _ name _) ->
               Set.insert name bound
             _ ->
               bound
           restDeps = findDepBlock bound' rest
       in stmtDeps ++ restDeps

instance Depencencies (E.Expression a) where
  findDependencies bound exp = case exp of
    E.Paren _ inner ->
      findDependencies bound inner
    E.Val _ val ->
      findDependencies bound val
    E.Unary _ _ inner ->
      findDependencies bound inner
    E.Binary _ _ l r ->
      findDependencies bound l ++ findDependencies bound r
    E.Call _ fn args ->
      let fnDeps = findDependencies bound fn
          argDeps = concat $ map (findDependencies bound) args
      in fnDeps ++ argDeps
    E.Cast _ _ inner ->
      findDependencies bound inner
    E.Var _ name ->
      if Set.member name bound then [] else [name]
    E.Access _ inner _ ->
      findDependencies bound inner

instance Depencencies (E.Value a) where
  findDependencies bound val = case val of
    E.StructVal _ _ fields ->
      concat $ map (findDependencies bound . snd) fields
    _ ->
      []

data InferState
  = InferState
    { nextVarN :: Int
    , currentSub :: Substitution }
  deriving (Show)

startingInferState :: InferState
startingInferState =
  InferState { nextVarN = 0, currentSub = Map.empty }

-- `Either Error` = `Result`, but somehow that
-- type synonym isn't allowed here.
type InferM = StateT InferState (Either Error)

type Environment = Map String Scheme

-- TODO: Make this a proper instance of the Types.Types class
applyEnv :: Substitution -> Environment -> Environment
applyEnv sub env = Map.map (apply sub) env

-- TODO: this should also start with prelude and imported names
startingEnv :: Environment
startingEnv =
  Map.fromList
  [ ("print", Scheme 1 (TFunc [TGen 1] tUnit)) ]

runInfer :: Monad m => StateT InferState m a -> m a
runInfer f = evalStateT f startingInferState

inferErr :: Error -> InferM a
inferErr err = lift $ Left err

newTypeVar :: InferM Type
newTypeVar = do
  st <- get
  let n = nextVarN st
  put $ st { nextVarN = 1 + n }
  return $ TVar $ "_v" ++ show n

getSub :: InferM Substitution
getSub = currentSub <$> get

extendSub :: Substitution -> InferM ()
extendSub sub = do
  s1 <- getSub
  let s = composeSubs sub s1
  modify (\st -> st { currentSub=s })

inferGroups :: [BindGroup a] -> Environment -> InferM (TypedDecls a, Environment)
inferGroups []     _   =
  return ([], Map.empty)
inferGroups (g:gs) env = do
  (typed, env1) <- inferGroup g env
  (rest, env2) <- inferGroups gs (Map.union env1 env)
  return (typed ++ rest, Map.union env1 env2)

inferGroup :: BindGroup a -> Environment -> InferM (TypedDecls a, Environment)
inferGroup (BindGroup impls) env = do
  -- Map each binding to a new type variable while recursively typing these bindings
  ts <- mapM (\_ -> newTypeVar) impls
  let bindingNames = map fst impls
  let bindingSchemes = map asScheme ts
  let groupBindings = Map.fromList $ zip bindingNames bindingSchemes
  let groupEnv = Map.union groupBindings env

  -- Do the actual inference
  typedDecls <- inferDecls groupEnv impls ts

  -- Apply the substitution to all the types and generalize them to schemes
  sub <- getSub
  let subbed = map (apply sub) ts
  let schemes = map (generalize (applyEnv sub env)) subbed
  let resultEnv = Map.fromList $ zip bindingNames schemes

  return (typedDecls, resultEnv)


{-
showTrace :: (Show a) => String -> a -> a
showTrace s a = trace (s ++ ": " ++ show a) a
-}

inferDecls :: Environment -> [(String, D.Declaration a)] -> [Type] -> InferM (TypedDecls a)
inferDecls env decls ts = mapM infer (zip decls ts)
  where infer ((name, decl), t) = do
          d <- inferDecl env decl
          unify t (getType d)
          return (name, d)

generalize :: Environment -> Type -> Scheme
generalize env t =
  let envVars = foldl Set.union Set.empty $ map (freeTypeVars . snd) $ Map.toList env
      freeVars = map TVar $ Set.toList $ Set.difference (freeTypeVars t) envVars
      genVars = map TGen [1..]
      sub = Map.fromList $ zip freeVars genVars
  in Scheme (length freeVars) (apply sub t)


instantiate :: Scheme -> InferM Type
instantiate sch@(Scheme n t) = do
  let range = [1..n]
  newVars <- mapM (\_ -> newTypeVar) range
  let genVars = map TGen range
  let sub = Map.fromList $ zip genVars newVars
  let applied = apply sub t
  verifyContainsNoGenerics sch applied
  return applied


verifyContainsNoGenerics :: Scheme -> Type -> InferM ()
verifyContainsNoGenerics sch t
  | containsGenerics t =
    let message = "instantiated type " ++ show t ++ " from " ++ show sch ++  " contains generics"
    in inferErr $ CompilerBug message
  | otherwise          =
    return ()


containsGenerics :: Type -> Bool
containsGenerics t = case t of
  TCon _ ts   -> any containsGenerics ts
  TFunc as t' -> any containsGenerics as || containsGenerics t'
  TVar _      -> False
  TGen _      -> True


inferDecl :: Environment -> D.Declaration a -> InferM (D.Declaration (Type, a))
inferDecl env decl = case decl of
  D.Let a name expr -> do
    expr' <- inferExpr env expr
    return $ D.Let (getType expr', a) name expr'

  D.Function a name args stmt -> do
    argTs <- mapM (\_ -> newTypeVar) args
    retT <- newTypeVar
    let argEnv = Map.fromList $ zip args (map asScheme argTs)
    let env' = Map.union argEnv env
    (stmt', stmtReturns) <- inferStmt env' stmt
    let funcReturns = toFunctionReturns stmtReturns
    unifyAll retT funcReturns
    sub <- getSub
    let argTypes = map (apply sub) argTs
    let returnT = apply sub retT
    let t = TFunc argTypes returnT
    return $ D.Function (t, a) name args stmt'

  D.TypeDef _ _ _ ->
    inferErr $ CompilerBug "TypeDefs are not bindings"

inferStmt :: Environment -> S.Statement a ->
             InferM (S.Statement (Type, a), DoesReturn)
inferStmt env stmt = case stmt of
  S.Return a Nothing ->
    return (S.Return (tUnit, a) Nothing, AlwaysReturns [tUnit])
  S.Return a (Just expr) -> do
    expr' <- inferExpr env expr
    return (S.Return (tUnit, a) (Just expr'), AlwaysReturns [getType expr'])

  S.Let a name expr -> do
    -- TODO: recursive binding?
    expr' <- inferExpr env expr
    return (S.Let (tUnit, a) name expr', NeverReturns)

  S.Assign a names expr ->
    case names of
     []    -> inferErr $ CompilerBug "assignment to no names"
     [var] -> do
       expr' <- inferExpr env expr
       let exprT = getType expr'
       sch <- lookupName var env
       varT <- instantiate sch
       -- TODO: not quite right, since this may result in too narrow of a type
       -- getting assigned, but it's good enough for now
       unify exprT varT
       return (S.Assign (tUnit, a) names expr', NeverReturns)
     _     -> error "TODO: Multi-part assignment statements"

  S.Block a stmts -> do
    (stmts', retT) <- inferBlock env stmts
    return (S.Block (tUnit, a) stmts', retT)

  S.Expr a expr -> do
    expr' <- inferExpr env expr
    return (S.Expr (tUnit, a) expr', NeverReturns)

  S.If a test blk els -> do
    testExpr <- inferExpr env test
    unify (getType testExpr) tBool
    (blk', blkReturns) <- inferBlock env blk
    (els', elsReturns) <- case els of
      Nothing   -> return (Nothing, NeverReturns)
      Just st -> do
        (stmt', retT) <- inferStmt env st
        return (Just stmt', retT)
    let ifReturns = combineReturns blkReturns elsReturns
    return (S.If (tUnit, a) testExpr blk' els', ifReturns)

  S.While a test blk -> do
    testExpr <- inferExpr env test
    unify (getType testExpr) tBool
    (blk', blkReturns) <- inferBlock env blk
    let whileReturns = demoteReturns blkReturns
    return (S.While (tUnit, a) testExpr blk', whileReturns)

inferBlock :: Environment -> [S.Statement a] ->
              InferM ([S.Statement (Type, a)], DoesReturn)
inferBlock _   []     = return ([], NeverReturns)
inferBlock env [stmt] = do
  (stmt', retT) <- inferStmt env stmt
  return ([stmt'], retT)
inferBlock env (s:ss) = do
  (s', _) <- inferStmt env s
  let env' = case s' of
        S.Let _ name expr ->
          let sch = generalize env (getType expr)
          in Map.insert name sch env
        _                -> env
  (ss', retT) <- inferBlock env' ss
  return $ (s' : ss', retT)

data DoesReturn
  = NeverReturns
  | SometimesReturns [Type]
  | AlwaysReturns [Type]
  deriving (Eq, Show)

combineReturns :: DoesReturn -> DoesReturn -> DoesReturn
combineReturns r1 r2 = case (r1, r2) of
  (NeverReturns, NeverReturns) ->
    NeverReturns
  (AlwaysReturns t1, AlwaysReturns t2) ->
    AlwaysReturns $ t1 ++ t2
  _ ->
    SometimesReturns $ getReturnTypes r1 ++ getReturnTypes r2

getReturnTypes :: DoesReturn -> [Type]
getReturnTypes NeverReturns          = []
getReturnTypes (SometimesReturns ts) = ts
getReturnTypes (AlwaysReturns ts)    = ts

demoteReturns :: DoesReturn -> DoesReturn
demoteReturns (AlwaysReturns ts) = SometimesReturns ts
demoteReturns ds                 = ds

toFunctionReturns :: DoesReturn -> [Type]
toFunctionReturns (AlwaysReturns ts) = ts
toFunctionReturns ds                 =
  -- Add tUnit to handle the case where the execution "fall off" the end of the
  -- function, since the return statements don't cover every case.
  tUnit : getReturnTypes ds


inferExpr :: Environment -> E.Expression a -> InferM (E.Expression (Type, a))
inferExpr env expr = case expr of
  E.Paren a e -> do
    e' <- inferExpr env e
    let t = getType e'
    return $ E.Paren (t, a) e'

  E.Val a val -> do
    val' <- inferValue env val
    let t = getType val'
    return $ E.Val (t, a) val'

  E.Unary a op exp -> do
    resultT <- newTypeVar
    exp' <- inferExpr env exp
    let expT = getType exp'
    let fnT = getUnaryFnType op
    unify fnT (TFunc [expT] resultT)
    sub <- getSub
    let t = apply sub resultT
    return $ E.Unary (t, a) op exp'

  E.Binary a op l r -> do
    resultT <- newTypeVar
    l' <- inferExpr env l
    let lt = getType l'
    r' <- inferExpr env r
    let rt = getType r'
    let fnT = getBinaryFnType op
    unify fnT (TFunc [lt, rt] resultT)
    sub <- getSub
    let t = apply sub resultT
    return $ E.Binary (t, a) op l' r'

  E.Call a fexp args -> do
    resultT <- newTypeVar
    fexp' <- inferExpr env fexp
    let ft = getType fexp'
    args' <- mapM (inferExpr env) args
    let argTs = map getType args'
    unify ft (TFunc argTs resultT)
    sub <- getSub
    let t = apply sub resultT
    return $ E.Call (t, a) fexp' args'

  E.Cast a t exp -> do
    exp' <- inferExpr env exp
    let expT = getType exp'
    sch <- attemptCast t expT
    return $ E.Cast (sch, a) t exp'

  E.Var a name -> do
    sch <- lookupName name env
    t <- instantiate sch
    return $ E.Var (t, a) name

  E.Access _ exp field -> do
    exp' <- inferExpr env exp
    let _ = getType exp'
    let _ = field
    return $ error "TODO: Infer for Access"

inferValue :: Environment -> E.Value a -> InferM (E.Value (Type, a))
inferValue _ val = case val of
  E.StrVal a str ->
    return $ E.StrVal (tString, a) str
  E.BoolVal a b ->
    return $ E.BoolVal (tBool, a) b
  E.IntVal a i ->
    return $ E.IntVal (tInt, a) i
  E.FloatVal a f ->
    return $ E.FloatVal (tFloat, a) f
  E.StructVal _ _ _ ->
    error "TODO: Infer for StructVal"


lookupName :: String -> Environment -> InferM Scheme
lookupName name env = case Map.lookup name env of
  Nothing  -> inferErr $ UndefinedVar name
  Just sch -> return sch

attemptCast :: String -> Type -> InferM Type
attemptCast toTypeName fromType =
  let toType = TCon toTypeName []
  in if canCast fromType toType
     then return toType
     else inferErr $ CannotCast $ "cannot cast " ++ show fromType ++ " to " ++ toTypeName

canCast :: Type -> Type -> Bool
canCast t1 t2
  | t1 == tInt && t2 == tFloat = True
  | t1 == tFloat && t2 == tInt = True
  | t2 == tString              = True
  | otherwise = False

getUnaryFnType :: UnaryOp -> Type
getUnaryFnType op = case op of
  BitInvert -> TFunc [tInt] tInt
  BoolNot   -> TFunc [tBool] tBool

getBinaryFnType :: BinOp -> Type
getBinaryFnType op
  | op `elem` intBinaryFuncs =
    TFunc [tInt, tInt] tInt
  | op `elem` numBinaryFuncs =
    TFunc [tInt, tInt] tInt -- TODO: fix this once there are typeclasses
  | op `elem` boolBinaryFuncs =
    TFunc [tBool, tBool] tBool
  | op `elem` equalityFuncs =
    TFunc [tInt, tInt] tBool -- TODO: fix this once there are typeclasses
  | op `elem` ordFuncs =
    TFunc [tInt, tInt] tBool -- TODO: fix this once there are typeclasses
  | otherwise =
    error $ "getBinaryFnType missing a case for " ++ show op

intBinaryFuncs :: [BinOp]
intBinaryFuncs = [Mod, BitAnd, BitOr, BitXor, LShift, RShift]

numBinaryFuncs :: [BinOp]
numBinaryFuncs = [Plus, Minus, Times, Divide, Power]

boolBinaryFuncs :: [BinOp]
boolBinaryFuncs = [BoolAnd, BoolOr]

equalityFuncs :: [BinOp]
equalityFuncs = [Eq, NotEq]

ordFuncs :: [BinOp]
ordFuncs = [Less, LessEq, Greater, GreaterEq]

unifyAll :: Type -> [Type] -> InferM ()
unifyAll t ts = do
  _ <- mapM (unify t) ts
  return ()

unify :: Type -> Type -> InferM ()
unify t1 t2 = do
  s <- getSub
  let message = "mgu " ++ (show $ apply s t1) ++ " " ++ (show $ apply s t2)
  s2 <- lift $ traceErr message $ mgu (apply s t1) (apply s t2)
  extendSub s2

mismatch :: Type -> Type -> Result a
mismatch t1 t2 = Left $ Mismatch t1 t2

-- TODO: remove debugging:
traceErr :: String -> Result a -> Result a
traceErr _       (Right x) = (Right x)
--traceErr message (Left x)  = trace message (Left x)
traceErr _ (Left x)  = (Left x)


unifies :: Type -> Type -> Bool
unifies t1 t2 = isRight $ mgu t1 t2


mgu :: Type -> Type -> Result Substitution
mgu t1 t2 = case (t1, t2) of
  (TGen _, _) ->
    --Left $ CompilerBug "A generic variable should have been instantiated"
    error "A generic variable should have been instantiated"
  (_, TGen _) ->
    --Left $ CompilerBug "A generic variable should have been instantiated"
    error "A generic variable should have been instantiated"

  (TCon ac ats, TCon bc bts) | ac == bc && length ats == length bts ->
    mguList emptySubstitution (zip ats bts)

  (TFunc aargs aret, TFunc bargs bret) | length aargs == length bargs -> do
    sub <- mguList emptySubstitution (zip aargs bargs)
    sub2 <- mgu (apply sub aret) (apply sub bret)
    return $ composeSubs sub sub2

  (TVar var, other) ->
    varBind var other

  (other, TVar var) ->
    varBind var other

  _ ->
    mismatch t1 t2

varBind :: String -> Type -> Result Substitution
varBind var other
  | other == TVar var =
    return emptySubstitution
  | Set.member var (freeTypeVars other) =
    Left $ InfiniteType var
  | otherwise =
    return $ Map.singleton (TVar var) other
-- TODO: check kinds in varBind

mguList :: Substitution -> [(Type, Type)] -> Result Substitution
mguList sub [] = return sub
mguList sub ((t1,t2):ts) = do
  sub2 <- mgu (apply sub t1) (apply sub t2)
  mguList (composeSubs sub sub2) ts

getType :: (Annotated a) => a (b, c) -> b
getType node = fst $ getAnnotation node

mustLookup :: (Ord k, Show k) => k -> Map k v -> v
mustLookup key m = case Map.lookup key m of
  Just val -> val
  Nothing  -> error $ "nothing bound for " ++ show key

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe d Nothing  = d


isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False
