module Inference (inferModule, mgu) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.State (StateT, modify, get, put, lift, evalStateT)

import AST.Annotation (Annotated, getAnnotation)
import AST.Expression (UnaryOp, BinOp)
import AST.Expression as E
import AST.Statement as S
import AST.Declaration as D

import Types
  ( Substitution
  , Scheme(..)
  , Type(..)
  , tInt
  , tFloat
  , tBool
  , tChar
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
  ( Module(..), bindings, types )
import Util.Graph
  ( components )

data BindGroup a
  = BindGroup
    { implicitBindings :: [(String, D.Declaration a)] }
    -- TODO: Add explicit bindings

-- (Type, a) adds a type to whatever the original annotation was
inferModule :: Module a -> Result (Module (Scheme, a))
inferModule m = do
  let bindGroups = makeBindGroups m
  binds <- runInfer $ inferGroups bindGroups startingEnv
  return $ Module { bindings=Map.fromList binds, types=(types m) }

makeBindGroups :: Module a -> [BindGroup a]
makeBindGroups m = --return [BindGroup $ Map.toList $ bindings m]
  let declarations = bindings m
      graph = gatherGraph declarations
      topoOrder = reverse $ components graph
      getBinding name = (name, mustLookup name declarations)
  in map BindGroup $ map (map getBinding) topoOrder

-- TODO: extend this into prelude (plus imported names)
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
    D.Let _ name _ exp ->
      findDependencies (Set.insert name bound) exp
    D.Function _ name _ args stmt ->
      findDependencies (Set.union bound $ Set.fromList (name:args)) stmt
    D.TypeDef _ _ _ ->
      []

instance Depencencies (S.Statement a) where
  findDependencies bound stmt = case stmt of
    S.Return _ mexp ->
      fromMaybe [] $ fmap (findDependencies bound) mexp
    S.Let _ name _ exp ->
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
             (S.Let _ name _ _) ->
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

-- TODO: this should also start with prelude and imported names
startingEnv :: Environment
startingEnv = Map.empty

runInfer f = evalStateT f startingInferState

newTypeVar :: InferM Type
newTypeVar = do
  st <- get
  let n = nextVarN st
  put $ st { nextVarN = 1 + n }
  return $ TVar $ "_v" ++ show n

inferGroups :: [BindGroup a] -> Environment -> InferM [(String, D.Declaration (Scheme, a))]
inferGroups []     _   =
  return []
inferGroups (g:gs) env = do
  typed <- inferGroup g env
  -- TODO: Need to pass type environment between these
  let bindings = toBindings typed
  let env' = Map.union (Map.fromList bindings) env
  rest <- inferGroups gs env'
  return $ typed ++ rest

inferGroup :: BindGroup a -> Environment -> InferM [(String, D.Declaration (Scheme, a))]
inferGroup (BindGroup impl) env = mapM addType' impl
  -- TODO: replace this with a real implementation
  where addType' (n, d) = do
          d' <- addType d
          return (n, d')

generalize :: Environment -> Type -> Scheme
generalize env t =
  let envVars = foldl Set.union Set.empty $ map (freeTypeVars . snd) $ Map.toList env
      freeVars = map TVar $ Set.toList $ Set.difference (freeTypeVars t) envVars
      genVars = map TGen [1..]
      sub = Map.fromList $ zip freeVars genVars
  in Scheme (length freeVars) (apply sub t)


instantiate :: Scheme -> InferM Type
instantiate (Scheme n t) = do
  newVars <- mapM (\_ -> newTypeVar) [1..n]
  let genVars = map TGen [1..n]
  let sub = Map.fromList $ zip genVars newVars
  return $ apply sub t

inferExpr :: Environment -> Expression a -> InferM (Expression (Scheme, a))
inferExpr env expr = case expr of
  Paren a e -> do
    e' <- inferExpr env e
    let sch = getScheme e'
    return $ Paren (sch, a) e'

  Val a val -> do
    val' <- inferValue env val
    let sch = getScheme val'
    return $ Val (sch, a) val'

  Unary a op exp -> do
    resultT <- newTypeVar
    exp' <- inferExpr env exp
    expT <- instantiate $ getScheme exp' --TODO: Switch back to storing types instead of schemes?
    let fnT = getUnaryFnType op
    sub <- lift $ mgu fnT (TFunc [expT] resultT) -- TODO: these should update the current substitution
    let t = apply sub resultT
    let sch = generalize env t
    return $ Unary (sch, a) op exp'

  Binary a op l r -> do
    resultT <- newTypeVar
    l' <- inferExpr env l
    r' <- inferExpr env r
    lt <- instantiate $ getScheme l'
    rt <- instantiate $ getScheme r'
    let fnT = getBinaryFnType op
    sub <- lift $ mgu fnT (TFunc [lt, rt] resultT)
    let t = apply sub resultT
    let sch = generalize env t
    return $ Binary (sch, a) op l' r'

  _ -> error "TODO: should this be returning the type as well? And why are these schemes?"

inferValue :: Environment -> Value a -> InferM (Value (Scheme, a))
inferValue env val = case val of
  StrVal a str ->
    return $ StrVal (asScheme tString, a) str
  BoolVal a b ->
    return $ BoolVal (asScheme tBool, a) b
  FloatVal a f ->
    return $ FloatVal (asScheme tFloat, a) f
  _ ->
    error "TODO: Infer for StructVal"

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
intBinaryFuncs = [Mod, BitAnd, BitOr, BitXor, LShift, RShift, RRShift]

numBinaryFuncs :: [BinOp]
numBinaryFuncs = [Plus, Minus, Times, Divide, Power]

boolBinaryFuncs :: [BinOp]
boolBinaryFuncs = [BoolAnd, BoolOr]

equalityFuncs :: [BinOp]
equalityFuncs = [Eq, NotEq]

ordFuncs :: [BinOp]
ordFuncs = [Less, LessEq, Greater, GreaterEq]

todoType :: Scheme
todoType = Scheme 0 $ TCon "TODO" []

toBindings :: [(String, D.Declaration (Scheme, a))] -> [(String, Scheme)]
toBindings typed = mapSnd getScheme typed

-- TODO: Replace with actual type inference
addType :: D.Declaration a -> InferM (D.Declaration (Scheme, a))
addType (D.Let a name t expr) = do
  expr' <- addTypeE expr
  return $ D.Let (todoType, a) name t expr'
addType (D.Function a name t args stmt) = do
  stmt' <- addTypeS stmt
  return $ D.Function (todoType, a) name t args stmt'
addType (TypeDef _ name td) =
  lift $ Left $ CompilerBug $ show $ TypeDef () name td

addTypeE :: E.Expression a -> InferM (E.Expression (Scheme, a))
addTypeE expr = case expr of
  Paren   a e -> do
    e' <- addTypeE e
    return $ Paren (todoType, a) e'
  Val     a val -> do
    val' <- addTypeV val
    return $ Val (todoType, a) val'
  Unary   a op e -> do
    e' <- addTypeE e
    return $ Unary (todoType, a) op e'
  Binary  a op l r -> do
    l' <- addTypeE l
    r' <- addTypeE r
    return $ Binary (todoType, a) op l' r'
  Call    a fn es -> do
    fn' <- addTypeE fn
    es' <- mapM addTypeE es
    return $ Call (todoType, a) fn' es'
  Cast    a t e -> do
    e' <- addTypeE e
    return $ Cast (todoType, a) t e'
  Var     a name -> do
    return $ Var (todoType, a) name
  Access  a e field -> do
    e' <- addTypeE e
    return $ Access (todoType, a) e' field

addTypeS :: S.Statement a -> InferM (S.Statement (Scheme, a))
addTypeS stmt = case stmt of
  Return a mexp -> do
    e <- case mexp of
      Nothing -> return Nothing
      Just e  -> Just <$> addTypeE e
    return $ Return (todoType, a) e
  S.Let a name t e -> do
    e' <- addTypeE e
    return $ S.Let (todoType, a) name t e'
  Assign a path e -> do
    e' <- addTypeE e
    return $ Assign (todoType, a) path e'
  Block a stmts -> do
    stmts' <- mapM addTypeS stmts
    return $ Block (todoType, a) stmts'
  Expr a e -> do
    e' <- addTypeE e
    return $ Expr (todoType, a) e'
  If a t st est -> do
    t' <- addTypeE t
    st' <- mapM addTypeS st
    est' <- case est of
      Nothing -> return Nothing
      Just ss -> Just <$> addTypeS ss
    return $ If (todoType, a) t' st' est'
  While a e ss -> do
    e' <- addTypeE e
    ss' <- mapM addTypeS ss
    return $ While (todoType, a) e' ss'

addTypeV :: E.Value a -> InferM (E.Value (Scheme, a))
addTypeV val = case val of
  StrVal     a name ->
    return $ StrVal (todoType, a) name
  BoolVal    a b ->
    return $ BoolVal (todoType, a) b
  IntVal     a i ->
    return $ IntVal (todoType, a) i
  FloatVal   a f ->
    return $ FloatVal (todoType, a) f
  StructVal  a name fields -> do
    fields' <- mapM (\(n,e) -> do { e' <- addTypeE e; return (n,e') }) fields
    return $ StructVal (todoType, a) name fields'

mismatch :: Type -> Type -> Result a
mismatch t1 t2 = Left $ Mismatch t1 t2

mgu :: Type -> Type -> Result Substitution
mgu t1 t2 = case (t1, t2) of
  (TGen _, _) ->
    Left $ CompilerBug "A generic variable should have been instantiated"
  (_, TGen _) ->
    Left $ CompilerBug "A generic variable should have been instantiated"

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

getScheme :: (Annotated a) => a (Scheme, b) -> Scheme
getScheme node = fst $ getAnnotation node

mustLookup :: (Ord k, Show k) => k -> Map k v -> v
mustLookup key m = case Map.lookup key m of
  Just val -> val
  Nothing  -> error $ "nothing bound for " ++ show key

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe d Nothing  = d

mapSnd :: (a -> b) -> [(x, a)] -> [(x, b)]
mapSnd _ []         = []
mapSnd f ((x,a):xs) = (x, f a) : mapSnd f xs
