module Main where

import Control.Monad (liftM)
import Data.Either (isLeft)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit ( Assertion, Test(..), (~:), (~?=), assertFailure )
import Test.Framework (defaultMain)

import TestUtil ( asGroup )

import Parse ( LiteralValue (..)
             , Expression (..)
             , Statement (..)
             )
import TypeInference
import InferExpression

main = defaultMain $
       asGroup [ ("getTypeForVar", testGetTypeForVar)
               , ("getFullTypeForVar", testGetFullTypeForVar)
               , ("equalityPairsFromSet", testEqualityPairsFromSet)
               , ("non-generic inference", testNonGeneric)
               , ("generic inference", testGenericTI)
               , ("expression inferences", testExprTI)
               , ("build fn body", testBuildFnBody)
               ]

emptyResult :: InfResult
emptyResult = (Map.empty, Map.empty)

assertFails (Left _)  = return ()
assertFails (Right x) = assertFailure (show x)

nilTN = TypeNode "()" []
strTN = TypeNode "String" []
intTN = TypeNode "Int" []
pairTN l r = TypeNode "Pair" [l, r]
listTN l = TypeNode "List" [l]

exampleResult1 :: InfResult
exampleResult1 = (Map.fromList [(1, strTN), (2, intTN),
                                (101, TypeNode "List" [1]), (102, TypeNode "Set" [10])],
                  Map.fromList [(3, 2), (10, 12), (5, 1)])

testGetTypeForVar =
  TestList [ "missing type var" ~: getTypeForVar emptyResult 1 ~?= Nothing
           , "existing type var" ~: getTypeForVar exampleResult1 2 ~?= Just intTN
           , "replaced but missing" ~: getTypeForVar exampleResult1 10 ~?= Nothing
           , "replaced and defined" ~: getTypeForVar exampleResult1 5 ~?= Just strTN ]

testGetFullTypeForVar =
  TestList [ "simple type" ~: getFullTypeForVar exampleResult1 1 ~?=
             (Constructor "String" [])
           , "complex with var" ~: getFullTypeForVar exampleResult1 102 ~?=
             (Constructor "Set" [Var 10])
           , "complex with inner"~: getFullTypeForVar exampleResult1 101 ~?=
             (Constructor "List" [Constructor "String" []]) ]

doInfer :: [Rules -> Rules] -> ErrorS InfResult
doInfer = infer . makeRules

makeRules :: [Rules -> Rules] -> Rules
makeRules [] = emptyRules
makeRules (r:rs) = r (makeRules rs)

inferTypeFor :: TypeVar -> [Rules -> Rules] -> ErrorS (Maybe TypeNode)
inferTypeFor var rules = do
  result <- doInfer rules
  return $ getTypeForVar result var

testNonGeneric =
  TestList [ "empty" ~:
             infer emptyRules ~?= Right (Map.empty, Map.empty)

           , "specify" ~:
             infer (specify 1 intTN emptyRules) ~?=
             Right (Map.fromList [(1, intTN)], Map.empty)

           , "equal" ~:
             infer (setEqual 1 2 emptyRules) ~?=
             Right (Map.empty, Map.fromList [(2, 1)])

           , "simple replacement" ~:
             doInfer [setEqual 1 2, specify 1 intTN] ~?=
             Right (Map.fromList [(1, intTN)], Map.fromList [(2, 1)])

           , "replace equal values" ~:
             doInfer [setEqual 1 2, specify 1 intTN, specify 2 intTN] ~?=
             Right (Map.fromList [(1, intTN)], Map.fromList [(2, 1)])

           , "catches incompatible sub" ~:
             isLeft (doInfer [setEqual 1 2, specify 1 intTN, specify 2 strTN]) ~?= True

           , "more complicated sub" ~:
             doInfer [specify 1 intTN, setEqual 3 4, setEqual 1 5, setEqual 1 2,
                      setEqual 5 2, setEqual 4 5] ~?=
             Right (Map.fromList [(1, intTN)], Map.fromList [(2, 1), (3, 1), (4, 1), (5, 1)])

           , "applies recursive equality" ~:
             doInfer [ specify 1 (pairTN 11 12), specify 2 (pairTN 21 22)
                     , specify 11 intTN, specify 22 strTN, setEqual 1 2 ] ~?=
             Right (Map.fromList [(1, (pairTN 11 22)), (11, intTN), (22, strTN)],
                    Map.fromList [(2, 1), (21, 11), (12, 22)])
           ]

testGenericTI =
  TestList ["rejects invalid generic relation" ~:
             isLeft (doInfer [instanceOf 1 2, specify 1 intTN, specify 2 strTN]) ~?= True

           , "applies generic relations" ~:
             inferTypeFor 2 [specify 1 intTN, instanceOf 2 1] ~?=
             Right (Just intTN)

           , "ignores reverse relationships" ~:
             inferTypeFor 2 [specify 1 intTN, instanceOf 1 2] ~?=
             Right Nothing

           , "accepts circular relationships" ~:
             inferTypeFor 2 [specify 1 intTN, instanceOf 2 1, instanceOf 1 2] ~?=
             Right (Just intTN)

           , "applies generics recursively" ~:
             doInfer [ specify 1 (pairTN 11 12), specify 2 (pairTN 21 22)
                     , specify 11 intTN, specify 22 strTN, instanceOf 1 2 ] ~?=
             Right (Map.fromList [ (1, pairTN 11 12), (2, pairTN 21 22)
                                 , (11, intTN), (12, strTN), (22, strTN) ],
                    Map.empty)

           , "allows multiple generic instantiations" ~:
             doInfer [ specify 1 (listTN 11), specify 2 (listTN 21), specify 3 (listTN 31)
                     , specify 21 intTN, specify 31 strTN
                     , instanceOf 2 1, instanceOf 3 1 ] ~?=
             Right (Map.fromList [ (1, listTN 11), (2, listTN 21), (3, listTN 31)
                                 , (21, intTN), (31, strTN) ],
                    Map.empty)

           , "applies equality constraint from generic" ~:
             inferTypeFor 22 [ specify 1 (TypeNode "Fn2" [11, 11, 12])
                             , specify 2 (TypeNode "Fn2" [21, 22, 23])
                             , specify 12 nilTN, specify 21 intTN
                             , instanceOf 2 1] ~?=
             Right (Just intTN)

           , "applies generics for multiple levels" ~:
             doInfer [specify 1 (listTN 11), specify 11 intTN, instanceOf 2 1, instanceOf 3 2] ~?=
             -- TODO: is this actually right? or should 2 and 3 have generic instances of 11?
             Right (Map.fromList [(1, listTN 11), (2, listTN 11), (3, listTN 11), (11, intTN)],
                    Map.empty)

           , "catches generic errors with separation" ~:
             isLeft (doInfer [ specify 1 (listTN 11), specify 11 intTN
                             , specify 4 (listTN 41), specify 41 strTN
                             , instanceOf 2 1, instanceOf 4 3, instanceOf 3 2 ]) ~?=
             True

           , "merges subcomponents" ~:
             doInfer [ specify 1 intTN, instanceOf 2 1, instanceOf 1 2, instanceOf 3 2]
             ~?= Right (Map.fromList [(1, intTN), (2, intTN), (3, intTN)], Map.fromList [(2, 1)])
           ]

emptyVarSet :: Set TypeVar
emptyVarSet = Set.empty

testEqualityPairsFromSet =
  TestList [ "empty" ~: equalityPairsFromSet emptyVarSet ~?= []
           , "1 elem" ~: equalityPairsFromSet (Set.singleton 1) ~?= []
           , "multiple elems" ~: equalityPairsFromSet (Set.fromList [1, 2, 3, 4]) ~?=
             [(2, 1), (3, 1), (4, 1)]
           ]

testExprTI =
  TestList [ "literal" ~: tiLiteralExpr
           , "typed expr" ~: tiTypedExpr
           , "typed expr mismatch" ~: tiTypedExprMismatch
           , "scope lookup" ~: testScopeLookup
           , "lookup var" ~: testLookupVar
           , "creates new scope" ~: testCreateScope
           , "application" ~: tiApplication
           , "let block" ~: tiLet
           , "multi-statement let" ~: tiMultiLet
           , "lambda expression" ~: tiLambdaExpr
           , "let with lambda" ~: tiLetWithLambda
           , "let polymorphism" ~: tiLetPolymorphism
           , "if statement" ~: tiIf
           , "if statement - mismatch" ~: tiIfMismatch
           , "if statement - not boolean" ~: tiIfNonBool
           , "mutual recursion" ~: tiMutualRecursion
           , "generic mutual recursion" ~: tiGenMutualRecursion
           ]

intType = Constructor "Int" []
floatType = Constructor "Float" []
boolType = Constructor "Bool" []
stringType = Constructor "String" []
intTE = TELit intType (LiteralInt 123)

tiLiteralExpr =
  let inferredType = do
        (tevar, tistate) <- gatherRules startingState intTE
        inferResult <- infer (tirules tistate)
        return $ getFullTypeForVar inferResult tevar
  in inferredType ~?= (Right intType)

tiTypedExpr =
  let te = TETyped intType intTE
      inferredType = do
        (tevar, tistate) <- gatherRules startingState te
        inferResult <- infer (tirules tistate)
        return $ getFullTypeForVar inferResult tevar
  in inferredType ~?= (Right intType)

tiTypedExprMismatch =
  let te = TETyped floatType intTE
      inferredType = do
        (tevar, tistate) <- gatherRules startingState te
        inferResult <- infer (tirules tistate)
        return $ getFullTypeForVar inferResult tevar
  in assertFails inferredType

testScopeLookup =
  let scopeVar = ScopedVar 123 True
      scope = Map.fromList [("times2", scopeVar)]
      scopes = Scopes { current=scope, parents=[] }
      result = scopeLookup "times2" scopes
  in result ~?= (Just scopeVar)

testLookupVar =
  let scopeVar = ScopedVar 123 True
      scope = Map.fromList [("times2", scopeVar)]
      scopes = Scopes { current=scope, parents=[] }
      tistate = startingState { tiscopes=scopes }
      result = lookupVar "times2" tistate
  in result ~?= (Right scopeVar)

testCreateScope =
  let scopeVar = ScopedVar 123 True
      tistate = createScope startingState [("times2", scopeVar)]
      result = lookupVar "times2" tistate
  in result ~?= (Right scopeVar)

tiApplication =
  let fnInScope = 123
      tistate = createScope startingState [("times2", ScopedVar fnInScope True)]
      te = TEAp (TEVar "times2") [intTE]
      inferredType = do
        (tevar, tistate') <- gatherRules tistate te
        inferResult <- infer (tirules tistate')
        return $ getFullTypeForVar inferResult tevar
  in inferredType ~?= Right (Var 1)

tiLet =
  let letBlock = TELet [("x", intTE)] (TEVar "x")
      inferredType = do
        (tevar, tistate) <- gatherRules startingState letBlock
        inferResult <- infer (tirules tistate)
        return $ getFullTypeForVar inferResult tevar
  in inferredType ~?= (Right intType)

tiMultiLet =
  let letBlock = TELet [ ("x", TEVar "y"), ("y", intTE) ] (TEVar "x")
      inferredType = do
        (tevar, tistate) <- gatherRules startingState letBlock
        inferResult <- infer (tirules tistate)
        return $ getFullTypeForVar inferResult tevar
  in inferredType ~?= (Right intType)

tiLambdaExpr =
  let lambdaExpr = TELam ["x"] (TEVar "x")
      inferredType = do
        (tevar, tistate) <- gatherRules startingState lambdaExpr
        inferResult <- infer (tirules tistate)
        return $ getFullTypeForVar inferResult tevar
  in inferredType ~?= (Right $ Constructor "Fn_1" [Var 2, Var 2])

tiLetWithLambda =
  let lambdaExpr = TELam ["x"] (TEVar "x")
      letBlock = TELet [("id", lambdaExpr)]  (TEAp (TEVar "id") [intTE])
      inferredType = do
        (tevar, tistate) <- gatherRules startingState letBlock
        inferResult <- infer (tirules tistate)
        return $ getFullTypeForVar inferResult tevar
  in inferredType ~?= (Right intType)

tiLetPolymorphism =
  let lambdaExpr = TELam ["x"] (TEVar "x")
      letBlock = TELet [("id", lambdaExpr)]  (TEAp (TEAp (TEVar "id") [TEVar "id"]) [intTE])
      inferredType = do
        (tevar, tistate) <- gatherRules startingState letBlock
        inferResult <- infer (tirules tistate)
        return $ getFullTypeForVar inferResult tevar
  in inferredType ~?= (Right intType)

tiIf =
  -- TODO: using a LiteralInt is a bit of a hack because this doesn't properly support
  -- record types yet.
  let test = TELit boolType (LiteralInt 1)
      ifCase = TELit stringType (LiteralString "foo")
      elseCase = TELit stringType (LiteralString "bar")
      ifBlock = TEIf test ifCase elseCase
      inferredType = do
        (tevar, tistate) <- gatherRules startingState ifBlock
        inferResult <- infer (tirules tistate)
        return $ getFullTypeForVar inferResult tevar
  in inferredType ~?= (Right stringType)

tiIfMismatch =
  let test = TELit boolType (LiteralInt 1)
      ifCase = TELit stringType (LiteralString "foo")
      ifBlock = TEIf test ifCase intTE
      inferredType = do
        (tevar, tistate) <- gatherRules startingState ifBlock
        inferResult <- infer (tirules tistate)
        return $ getFullTypeForVar inferResult tevar
  in assertFails inferredType

tiIfNonBool =
  let ifCase = TELit stringType (LiteralString "foo")
      elseCase = TELit stringType (LiteralString "bar")
      ifBlock = TEIf intTE ifCase elseCase
      inferredType = do
        (tevar, tistate) <- gatherRules startingState ifBlock
        inferResult <- infer (tirules tistate)
        return $ getFullTypeForVar inferResult tevar
  in assertFails inferredType

tiMutualRecursion =
  let test = TELit boolType (LiteralInt 1)
      ifCase = intTE
      elseCase = TEAp (TEVar "g") []
      fLambda = TELam [] (TEIf test ifCase elseCase)
      gLambda = TELam [] (TEAp  (TEVar "f") [])
      letBlock = TELet [("f", fLambda), ("g", gLambda)] (TEVar "f")
      inferredType = do
        (tevar, tistate) <- gatherRules startingState letBlock
        inferResult <- infer (tirules tistate)
        return $ getFullTypeForVar inferResult tevar
  in inferredType ~?= (Right $ Constructor "Fn_0" [intType])


tiGenMutualRecursion =
  let test = TELit boolType (LiteralInt 1)
      ifCase = TEVar "x"
      elseCase = TEAp (TEVar "g") [TEVar "x"]
      fLambda = TELam ["x"] (TEIf test ifCase elseCase)
      gLambda = TELam ["y"] (TEAp  (TEVar "f") [TEVar "y"])
      letBlock = TELet [("f", fLambda), ("g", gLambda)] (TEVar "g")
      inferredType = do
        (tevar, tistate) <- gatherRules startingState letBlock
        inferResult <- infer (tirules tistate)
        return $ getFullTypeForVar inferResult tevar
  -- TODO: find a better way of testing the type variables in the returned function
  in inferredType ~?= (Right $ Constructor "Fn_1" [Var 13, Var 13])

testBuildFnBody =
  TestList [ "build return stmt" ~: buildReturnBlock
           , "no return" ~: blockWithoutReturn
           , "misplaced return" ~: misplacedReturn
           , "expr statement" ~: exprStatement
           , "let statement" ~: letStatement
           ]

buildReturnBlock =
  let statements = [StatementReturn $ ExpressionLit $ LiteralInt 99]
      result = buildFnBody statements
  in result ~?= Right (TELit intType (LiteralInt 99))

blockWithoutReturn = (buildFnBody []) ~?= (Right TENil)

misplacedReturn =
  let statements = [ StatementReturn $ ExpressionLit $ LiteralInt 99
                   , StatementExpr $ ExpressionLit $ LiteralInt 99 ]
      result = buildFnBody statements
  in assertFails result

exprStatement =
  let statements = [StatementExpr $ ExpressionLit $ LiteralInt 99]
      result = buildFnBody statements
  in result ~?= Right (TESeq (TELit intType (LiteralInt 99)) TENil)

letStatement =
  let statements = [ StatementLet "x" (ExpressionLit (LiteralInt 123))
                   , StatementExpr $ ExpressionVar "x" ]
      result = buildFnBody statements
  in result ~?= Right (TELet [("x", intTE)] (TESeq (TEVar "x") TENil))
