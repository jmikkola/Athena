module Inference (inferModule, mgu) where

--import Data.Map (Map)
import qualified Data.Map as Map
-- import Data.Set (Set)
import qualified Data.Set as Set

import AST.Expression as E
import AST.Statement as S
import AST.Declaration as D

import Types
  ( Substitution
  , Type(..)
  , apply
  , composeSubs
  , emptySubstitution
  , freeTypeVars )
import Errors
  ( Error(..)
  , Result )
import FirstPass
  ( Module(..), bindings, types )

data BindGroup a
  = BindGroup
    { implicitBindings :: [(String, D.Declaration a)] }
    -- TODO: Add explicit bindings

-- (Type, a) adds a type to whatever the original annotation was
inferModule :: Module a -> Result (Module (Type, a))
inferModule m = do
  bindGroups <- makeBindGroups m
  binds <- inferGroups bindGroups
  return $ Module { bindings=Map.fromList binds, types=(types m) }

-- TODO: Actually implement this
makeBindGroups :: Module a -> Result [BindGroup a]
makeBindGroups m = return [BindGroup $ Map.toList $ bindings m]

inferGroups :: [BindGroup a] -> Result [(String, D.Declaration (Type, a))]
inferGroups []     = return []
inferGroups (g:gs) = do
  binds <- inferGroup g
  -- TODO: Need to pass type environment between these
  rest <- inferGroups gs
  return $ binds ++ rest

inferGroup :: BindGroup a -> Result [(String, D.Declaration (Type, a))]
inferGroup (BindGroup impl) = mapM addType' impl
  -- TODO: replace this with a real implementation
  where addType' (n, d) = do
          d' <- addType d
          return (n, d')

todoType :: Type
todoType = TCon "TODO" []

-- TODO: Replace with actual type inference
addType :: D.Declaration a -> Result (D.Declaration (Type, a))
addType (D.Let a name t expr) = do
  expr' <- addTypeE expr
  return $ D.Let (todoType, a) name t expr'
addType (D.Function a name t args stmt) = do
  stmt' <- addTypeS stmt
  return $ D.Function (todoType, a) name t args stmt'
addType (TypeDef _ name td) =
  Left $ CompilerBug $ show $ TypeDef () name td

addTypeE :: E.Expression a -> Result (E.Expression (Type, a))
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

addTypeS :: S.Statement a -> Result (S.Statement (Type, a))
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

addTypeV :: E.Value a -> Result (E.Value (Type, a))
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
    return $ Map.singleton var other
-- TODO: check kinds in varBind

mguList :: Substitution -> [(Type, Type)] -> Result Substitution
mguList sub [] = return sub
mguList sub ((t1,t2):ts) = do
  sub2 <- mgu (apply sub t1) (apply sub t2)
  mguList (composeSubs sub sub2) ts
