module TypeCheck where

import Data.Either (lefts)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

import AST.Declaration (Declaraction, File)
import qualified AST.Declaration as Declaraction
import AST.Expression (Expression, Op, Value)
import qualified AST.Expression as E
import AST.Statement (Statement)
import qualified AST.Statement as Statement
import AST.Type (Type)
import qualified AST.Type as T

type Result = Either String

checkFile :: File -> Result ()
checkFile = undefined

checkDeclaration :: Declaraction -> Result ()
checkDeclaration _ = return ()

requireExprType :: Expression -> Type -> Result Type
requireExprType e t =
  case e of
   (E.EParen e')     -> requireExprType e' t
   (E.EValue v)      -> requireEqual t (valueType v)
   (E.EUnary o e')   -> do
     requireValidUnary o
     requireExprType e' t
   (E.EBinary o l r) -> do
     requireExprType l t
     requireExprType r t
   (E.ECall f args)  -> undefined -- need info about the function's type
   (E.ECast t' e')   -> do
     requireEqual t t'
     checkExpression e'
   (E.EVariable var) -> undefined -- need info about the variable's type

checkExpression :: Expression -> Result Type
checkExpression e =
  case e of
   (E.EParen e')     -> checkExpression e'
   (E.EValue v)      -> return $ valueType v
   (E.EUnary o e')   -> do
     requireValidUnary o
     checkExpression e'
   (E.EBinary o l r) -> do
     t <- checkExpression l
     requireExprType r t
   (E.ECall f args)  -> undefined -- need info about the function's type
   (E.ECast t' e')   -> do
     checkExpression e'
     return t'
   (E.EVariable var) -> undefined -- need info about the variable's type

valueType :: Value -> Type
valueType (E.EString _) = T.String
valueType (E.EBool _)   = T.Bool
valueType (E.EInt _)    = T.Int
valueType (E.EFloat _)  = T.Float

-- TODO: Split unary and binary ops into separate types
requireValidUnary :: Op -> Result ()
requireValidUnary o =
  case o of
   E.BitInvert -> return ()
   E.BoolNot   -> return ()
   otherwise    -> Left $ "not a valid unary operator " ++ show o

requireEqual :: Type -> Type -> Result Type
requireEqual t1 t2 =
  if t1 == t2 then return t1
  else Left $ "Type mismatch between " ++ show t1 ++ " and " ++ show t2
