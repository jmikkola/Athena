module Types where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- TODO: Add kinds to TCon and TVar

data Type
  = TCon String [Type]
  | TFunc [Type] Type
  | TVar String
  | TGen Int
  deriving (Eq, Show)

tcon0 :: String -> Type
tcon0 name = TCon name []

tInt    = tcon0 "Int"
tFloat  = tcon0 "Float"
tBool   = tcon0 "Bool"
tChar   = tcon0 "Char"
tString = tcon0 "String"
tUnit   = tcon0 "()"


type Substitution = Map String Type

emptySubstitution :: Substitution
emptySubstitution = Map.empty

-- `apply (composeSubs a b) t`
-- is equivalent to
-- `apply b (apply a t)`
composeSubs :: Substitution -> Substitution -> Substitution
composeSubs a b =
  Map.union (Map.map (apply b) a) b


class Types t where
  apply :: Substitution -> t -> t
  freeTypeVars :: t -> Set String

instance Types Type where
  apply sub (TCon con ts) =
    TCon con (apply sub ts)
  apply sub (TFunc args ret) =
    TFunc (apply sub args) (apply sub ret)
  apply sub (TVar tv) =
    case Map.lookup tv sub of
     Nothing -> TVar tv
     Just t' -> t'
  apply _   (TGen n) =
    TGen n

  freeTypeVars (TCon con ts) =
    freeTypeVars ts
  freeTypeVars (TFunc args ret) =
    Set.union (freeTypeVars args) (freeTypeVars ret)
  freeTypeVars (TVar tv) =
    Set.singleton tv
  freeTypeVars (TGen _) =
    Set.empty


instance (Types a) => Types [a] where
  apply sub lst = map (apply sub) lst
  freeTypeVars ts = foldl Set.union Set.empty (map freeTypeVars ts)


-- TODO: Add kinds to Scheme
data Scheme
  = Scheme Type
  deriving (Eq, Show)
