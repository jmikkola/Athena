module TypeInference where

import Data.Map (Map)
import qualified Data.Map as Map

type ErrorS = Either String

class Resolveable a where
  combine :: a -> a -> ErrorS a

data SimpleType a = STBool
                  | STInt
                  | STFloat
                  | STChar
                  | STList a
                  deriving (Show, Eq)

-- This isn't quite right
instance (Eq a, Show a) => Resolveable (SimpleType a) where
  combine a b =
    if a == b
    then Right a
    else case (a, b) of
      (a, b) -> Left ("Can't combine types " ++ (show a) ++ " and " ++ (show b))


data Relationship = Equivalent String String
                  | CompositeOf String String [String]
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
-- Equivalent "cons1.arg1" "cons2.arg2.subtype[0]" ?????
CompositeOf "cons1.ret" "List" ["cons1.arg1"]

-- OK, how about the internal representation of the types while it is working?

TypeVar a => _
TypeVar b => _
...

Relationship (Equivalent a b)

-- Do you ever need to use a fact in a relationship more than once?
-}
type TypeVar = String
type TypeVars = Map String (Maybe (SimpleType TypeVar))

lookupTVar :: TypeVar -> TypeVars -> ErrorS (Maybe (SimpleType TypeVar))
lookupTVar key vars = case Map.lookup key vars of
  Nothing -> Left ("Typevar " ++ key ++ " not found")
  Just st -> Right st

crunch :: Relationship -> TypeVars -> ErrorS TypeVars
crunch (Equivalent vara varb) tvars = do
  ta <- lookupTVar vara tvars
  tb <- lookupTVar varb tvars
  resultingType <- case (ta, tb) of
    (Nothing, Nothing) -> Right $ Nothing
    (Just a,  Nothing) -> Right $ Just a
    (Nothing, Just b)  -> Right $ Just b
    (Just a,  Just b)  -> case combine a b of
      Left err -> Left err
      Right t  -> Right $ Just t
  return (Map.insert varb resultingType (Map.insert vara resultingType tvars))

crunch (CompositeOf vara compositeKind compositeVars) tvars = do
  typea <- lookupTVar vara tvars
  Left "wtf"
  -- Wait, what was supposed to happen here?

tryCrunching = crunch (Equivalent "a" "b") (Map.fromList [("a", Nothing), ("b", Just STInt)])
