module TypeInference where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

--import Eval

type TypeVar = String
type TypeConstructor = String

-- Temporary - I hope
data Exp = EVar String
         | ELit Lit
         | EApp Exp Exp
         | EAbs String Exp
         | ELet String Exp Exp
         deriving (Eq, Ord, Show)

data Lit = LInt Int
         | LBool Bool
         | LFloat Float
         deriving (Eq, Ord, Show)

data Type = TVar TypeVar
          | TInt
          | TBool
          | TFloat
          | TFun Type Type
          | TCon TypeConstructor [Type]
          deriving (Eq, Ord, Show)

data Scheme = Scheme [TypeVar] Type
            deriving (Show)

type Substitution = Map TypeVar Type

newtype TypeEnv = TypeEnv (Map TypeVar Scheme)
                deriving (Show)

data TIEnv = TIEnv {}
           deriving (Show)

data TIState = TIState { tiSupply :: Int
                       , tiSubst :: Substitution }
             deriving (Show)

-- Heavy-duty monad stuff
type TI a = ErrorT String (ReaderT TIEnv (StateT TIState IO)) a

class Types a where
  freeTypeVars :: a -> Set TypeVar
  apply :: Substitution -> a -> a

instance Types Type where
  freeTypeVars t = case t of
    (TVar n)      -> Set.singleton n
    (TFun t1 t2)  -> Set.union (freeTypeVars t1) (freeTypeVars t2)
    (TCon _ ts)   -> foldl Set.union Set.empty (map freeTypeVars ts)
    _             -> Set.empty
  apply s t = case t of
    (TVar n)     -> case Map.lookup n s of
      Nothing -> TVar n
      Just t  -> t
    (TFun t1 t2) -> TFun (apply s t1) (apply s t2)
    (TCon c ts)  -> TCon c (map (apply s) ts)
    t            -> t

instance Types Scheme where
  -- Free type variables in t not listed in vars
  freeTypeVars (Scheme vars t) = Set.difference (freeTypeVars t) (Set.fromList vars)
  -- Original vars, but t has a modified s applied to it (modified by removing any variables
  -- from s found in vars)
  apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

instance Types TypeEnv where
  freeTypeVars (TypeEnv env) = freeTypeVars (Map.elems env)
  apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

instance Types a => Types [a] where
  freeTypeVars l = foldr Set.union Set.empty (map freeTypeVars l)
  apply s = map (apply s)

nullSubst :: Substitution
nullSubst = Map.empty

composeSubst :: Substitution -> Substitution -> Substitution
-- The union of s1 and the result of applying s1 to s2
composeSubst s1 s2 = Map.union (Map.map (apply s1) s2) s1

-- Does what it sounds like
remove :: TypeEnv -> TypeVar -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
  where vars = Set.toList (Set.difference (freeTypeVars t) (freeTypeVars env))

runTI :: TI a -> IO (Either String a, TIState)
runTI t =
  do
    (res, st) <- runStateT (runReaderT (runErrorT t) initTIEnv) initTIState
    return (res, st)
  where initTIEnv = TIEnv {}
        initTIState = TIState { tiSupply = 0, tiSubst = nullSubst }

newTyVar :: String -> TI Type
newTyVar prefix = do
  state <- get
  let varNum = tiSupply state
  put state { tiSupply = varNum + 1 }
  return (TVar (prefix ++ show varNum))

-- Substitutes variables in the scheme for new variables
instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
  -- trust that the language disallow writing typevars with the prefix '__v'
  nvars <- mapM (\_ -> newTyVar "__v") vars
  let s = Map.fromList (zip vars nvars)
  return $ apply s t

unifyError :: (Show a, Show b) => a -> b -> TI Substitution
unifyError a b = throwError error
  where error = "Types do not unify: " ++ show a ++ " and " ++ show b

mostGeneralUnifier :: Type -> Type -> TI Substitution
mostGeneralUnifier t1 t2 = case (t1, t2) of
  (TFun l r,  TFun l' r')  -> do
    s1 <- mostGeneralUnifier l l'
    s2 <- mostGeneralUnifier (apply s1 r) (apply s1 r')
    return $ composeSubst s1 s2
  (TVar u,    t)           -> varBind u t
  (t,         TVar u)      -> varBind u t
  (TInt,      TInt)        -> return nullSubst
  (TFloat,    TFloat)      -> return nullSubst
  (TBool,     TBool)       -> return nullSubst
  (TCon c ts, TCon c' ts') ->
    -- Is this logic right?
    if c /= c' || (length ts) /= (length ts')
    then unifyError t1 t2
    else mguList nullSubst ts ts'
--         do
--      ss <- mapM (\(a,b) -> mostGeneralUnifier a b) (zip ts ts')
--      return (foldl composeSubst nullSubst ss)
  _                        -> unifyError t1 t2


-- This might be the right logic for two TCon`s
mguList :: Substitution -> [Type] -> [Type] -> TI Substitution
mguList s []     []       = return s
mguList _ []     _        = error "This should have been detected earlier"
mguList _ _      []       = error "This should have been detected earlier"
mguList s (t:ts) (t':ts') = do
  s1 <- mostGeneralUnifier (apply s t) (apply s t')
  let s2 = composeSubst s1 s
  mguList s1 ts ts'

varBind :: TypeVar -> Type -> TI Substitution
varBind u t | t == TVar u                    = return nullSubst
            | Set.member u (freeTypeVars t) =
              throwError $ "occur check fails: " ++ u ++ " vs. " ++ show t
            | otherwise                     = return (Map.singleton u t)

tiLiteral :: TypeEnv -> Lit -> TI (Substitution, Type)
tiLiteral _ (LInt _)   = return (nullSubst, TInt)
tiLiteral _ (LFloat _) = return (nullSubst, TFloat)
tiLiteral _ (LBool _)  = return (nullSubst, TBool)

ti :: TypeEnv -> Exp -> TI (Substitution, Type)
ti (TypeEnv env) (EVar n)       = case Map.lookup n env of
  Nothing    -> throwError $ "unbound variable: " ++ n
  Just sigma -> do
     t <- instantiate sigma
     return (nullSubst, t)
ti env           (ELit l)       = tiLiteral env l
ti env           (EAbs n e)     = do
  tv <- newTyVar "__v"
  let TypeEnv env' = remove env n
      -- Isn't that just an insert?
      env'' = TypeEnv (Map.union env' (Map.singleton n (Scheme [] tv)))
  (s1, t1) <- ti env'' e
  return (s1, TFun (apply s1 tv) t1)
ti env           (EApp e1 e2)   = do
  tv <- newTyVar "__v"
  (s1, t1) <- ti env e1
  (s2, t2) <- ti (apply s1 env) e2
  s3 <- mostGeneralUnifier (apply s2 t1) (TFun t2 tv)
  return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
ti env           (ELet x e1 e2) = do
  (s1, t1) <- ti env e1
  let TypeEnv env' = remove env x
      t'           = generalize (apply s1 env) t1
      env''        = TypeEnv (Map.insert x t' env')
  (s2, t2) <- ti (apply s1 env'') e2
  return (composeSubst s1 s2, t2)

typeInference :: Map TypeVar Scheme -> Exp -> TI Type
typeInference env e = do
  (s, t) <- ti (TypeEnv env) e
  return $ apply s t
