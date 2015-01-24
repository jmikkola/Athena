module TypeInference where

-- My attempt at approximating Algorithm W without actually knowing how it works.

data ConcreteType = CTBool
                  | CTChar
                  | CTInt
                  | CTFloat
                  | CTList ConcreteType
                  | CTFunction [ConcreteType] ConcreteType
                  deriving (Read, Show, Eq)

data PartialType = KnownType ConcreteType
                 | TypeVariable String
                 | PTList PartialType
                 | PTFunction [PartialType] PartialType
                 deriving (Read, Show, Eq)

combineTypes :: PartialType -> PartialType -> Maybe PartialType
combineTypes (KnownType a) (KnownType b) = if a == b then Just (KnownType a) else Nothing

 -- TODO: mark that the variables are the same thing
combineTypes (TypeVariable a) (TypeVariable b) = Just (TypeVariable b)
 -- TODO: set what type variable means
combineTypes (TypeVariable s) (KnownType a) = Just (KnownType a)
combineTypes (TypeVariable a) (PTList b) = Just (PTList b)
combineTypes (TypeVariable a) (PTFunction argsb retb) = Just (PTFunction argsb retb)
combineTypes a b@(TypeVariable _) = combineTypes b a

combineTypes (PTList a) (PTList b) = do
  combo <- combineTypes a b
  return $ PTList combo

combineTypes (PTFunction argsa reta) (PTFunction argsb retb) =
  if (length argsa) /= (length argsb)
  then Nothing
  else do
    combinedArgs <- mapM (\(ta, tb) -> combineTypes ta tb) (zip argsa argsb)
    combinedRet <- combineTypes reta retb
    return $ PTFunction combinedArgs combinedRet



combineTypes _ _ = undefined
