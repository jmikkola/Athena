module TypeCheck where

import Control.Monad (foldM)
import Data.Either (lefts)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

import AST.Declaration (Declaraction, File)
import qualified AST.Declaration as D
import AST.Expression (Expression, Value)
import qualified AST.Expression as E
import AST.Statement (Statement)
import qualified AST.Statement as Statement
import AST.Type (Type)
import qualified AST.Type as T

type Result = Either String

type TypeScope = Map String Type

checkFile :: File -> Result ()
checkFile file = do
  fs <- buildFileScope file
  return ()

buildFileScope :: File -> Result TypeScope
buildFileScope = foldM addDecl Map.empty
  where addDecl ts decl =
          let t = declType decl
              n = declName decl
          in case Map.lookup n ts of
              (Just _) -> Left $ "duplicate definition of " ++ n
              Nothing  -> return $ Map.insert n t ts

declName :: Declaraction -> String
declName (D.Let n _ _) = n
declName (D.Function n _ _ _) = n

declType :: Declaraction -> Type
declType (D.Let _ t _) = t
declType (D.Function _ t _ _) = t

checkDeclaration :: TypeScope -> Declaraction -> Result ()
checkDeclaration _ _ = return ()

requireExprType :: TypeScope -> Expression -> Type -> Result Type
requireExprType ts e t =
  case e of
   (E.EParen e')     -> requireExprType ts e' t
   (E.EValue v)      -> requireEqual t (valueType v)
   (E.EUnary _ e')   -> requireExprType ts e' t
   (E.EBinary _ l r) -> do
     _ <- requireExprType ts l t
     requireExprType ts r t
   (E.ECall f args)  -> undefined -- need info about the function's type
   (E.ECast t' e')   -> do
     _ <- requireEqual t t'
     checkExpression ts e'
   (E.EVariable var) -> undefined -- need info about the variable's type

checkExpression :: TypeScope -> Expression -> Result Type
checkExpression ts e =
  case e of
   (E.EParen e')     -> checkExpression ts e'
   (E.EValue v)      -> return $ valueType v
   (E.EUnary o e')   -> checkExpression ts e'
   (E.EBinary o l r) -> do
     t <- checkExpression ts l
     requireExprType ts r t
   (E.ECall f args)  -> undefined -- need info about the function's type
   (E.ECast t' e')   -> do
     _ <- checkExpression ts e'
     return t'
   (E.EVariable var) -> undefined -- need info about the variable's type

valueType :: Value -> Type
valueType (E.EString _) = T.String
valueType (E.EBool _)   = T.Bool
valueType (E.EInt _)    = T.Int
valueType (E.EFloat _)  = T.Float

requireEqual :: Type -> Type -> Result Type
requireEqual t1 t2 =
  if t1 == t2 then return t1
  else Left $ "Type mismatch between " ++ show t1 ++ " and " ++ show t2
