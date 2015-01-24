module TypeInference where

import Data.Map (Map)

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


-- Here's more like the interface that I want

resolveTypes :: TypedSystem -> Either String (Map CodePoint ConcreteType)
resolveTypes = undefined

data TypedSystem = TypedSystem
                   { codePoints :: Map CodePoint TypeDetails
                   , relationships :: [Relationship]
                   } deriving (Show)

data TypeDetails = Known ConcreteType
                 | Unknown
                 deriving (Show)

type CodePoint = String
type CompositionType = String

data Relationship = Equivalent CodePoint CodePoint
                  | CompositeOf CompositionType CodePoint [CodePoint]
                  deriving (Show)

{-

(defn inc (n) (+ n 1))

Codepoint: "+" -> KnownType (Function [Int, Int] Int)
Codepoint: "inc" -> KnownType (Function [?, ?] ?)
Codepoint: "n" -> Unknown
Codepoint: "1" -> KnownType Int

or maybe smaller details:

Codepoint: "+.arg1" -> KnownType Int
Codepoint: "+.arg2" -> KnownType Int
Codepoint: "+.ret" -> KnownType Int
Codepoint: "inc.arg1" -> Unknown
Codepoint: "inc.ret" -> Unknown
Codepoint: "n" -> Unknown
Codepoint: "1" -> KnownType Int

Maybe it would be a good idea to separate supporting knowledge (like the fact that "1" as the type
Int) from the things that we are trying to figure out. That way, there would be less junk to return. Anyway. The Relationships:

Equivalent ("n") ("inc.arg1")
Equivalent ("n") ("+.arg1")
Equivalent ("+.rettype") ("inc.rettype")

And it should return (at least for the unknown things):

"inc.arg1" : Int
"inc.ret": Int
"n": Int

I think that there needs to be a slighly more complicated system than just
"equivalent"

(defn pair (a b) (cons a (cons b (list))))

Codepoint: "list.ret" -> KnownType (List a)
Codepoint: "cons.arg1" -> KnownType a
Codepoint: "cons.arg2" -> KnownType (List a) <-- or should some of this be encoded as relationships somehow?
Codepoint: "cons.ret" -> KnownType (List a)
Codepoint: "pair.arg1" -> Unknown
Codepoint: "pair.arg2" -> Unknown
Codepoint: "pair.ret" -> Unknown
Codepoint: "a" -> Unknown
Codepoint: "b" -> Unknown

Equivalent "pair.arg1" "a"
Equivalent "pair.arg2" "b"
Equivalent "pair.ret" ... Interesting. I guess that you need a separate codepoint for each invokation?

Maybe what this really requires is rules + codepoints + things to figure out

Equivalent "cons1.arg1" "a"
Equivalent "cons1.arg2" "cons2.ret"
Equivalent "cons1.ret" "pair.ret"
Equivalent "cons1.arg1" "cons2.arg2.subtype[0]" ?????
-}
