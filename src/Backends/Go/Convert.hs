module Backends.Go.Convert where

import Control.Monad (liftM, foldM)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Backends.Go.Syntax as Syntax
import qualified AST.Expression as E
import qualified Type as T
import Type (Type, TypeRef)
import IR

type Result = Either String

convertFile :: Map TypeRef Type -> [IR.Decl] -> Result [Syntax.Declaration]
convertFile types file = liftM concat $ mapM (convertDecl types) file

note :: a -> Maybe b -> Either a b
note msg = maybe (Left msg) Right

getType :: Map TypeRef Type -> TypeRef -> Result Type
getType types tref = note msg $ Map.lookup tref types
  where msg = "Compiler bug: missing type for reference " ++  tref

-- TODO: consider putting type map in a state monad
convertDecl :: Map TypeRef Type -> IR.Decl -> Result [Syntax.Declaration]
convertDecl types decl = case decl of
  IR.StmtDecl stmt -> case stmt of
    IR.Let name typ expr -> case expr of
      IR.Lambda t args stmt' -> do
        decl' <- makeFnDecl types t args
        body <- convertBlock types stmt'
        return [Syntax.Function name decl' body]
      _ -> do
        typ' <- convertType types typ
        expr' <- convertExpr types expr
        return [Syntax.Variable name (Just typ') expr']
    _ ->
      fail $ "cannot convert `" ++ show stmt ++ "` to a module-level declaration"
  IR.TypeDecl name typ -> case typ of
    T.Struct fields -> do
      fields' <- mapMSnd (convertType types) fields
      stringMethod <- makeStringMethod name fields'
      return [Syntax.Structure name fields', stringMethod]
    T.Enum options -> do
      structures <- makeStructures types name options
      let iface = Syntax.Interface name [(enumMethodName name, enumTagMethodDecl)]
      return $ iface : structures
    _ ->
      fail $ "cannot emit declaration for " ++ show typ

makeStringMethod :: String -> [(String, Syntax.Type)] -> Result Syntax.Declaration
makeStringMethod structName fields =
  let fieldNames = map fst fields
      decl = Syntax.FunctionDecl [] [Syntax.JustType $ Syntax.GoString]
      sprintf = Syntax.FieldAccess (Syntax.Var "fmt") "Sprintf"
      formatStr = structName ++ "{" ++ (concatMap (++ ": %v,") fieldNames) ++ "}"
      format = Syntax.StrVal formatStr
      values = map (Syntax.FieldAccess (Syntax.Var "self")) fieldNames
      printExpr = Syntax.Call sprintf (format : values)
      printStmt = Syntax.Return printExpr
      body = Syntax.Block [printStmt]
  in return $ Syntax.Method ("self", Syntax.GoStruct structName) "String" decl body

enumTagMethodDecl :: Syntax.FunctionDecl
enumTagMethodDecl = Syntax.FunctionDecl [] [Syntax.JustType Syntax.GoInt]

enumMethodName :: String -> String
enumMethodName = (++) "Enum"

makeStructures :: Map TypeRef Type -> String -> [(String, [(String, TypeRef)])]
               -> Result [Syntax.Declaration]
makeStructures types name options =
  (liftM concat) (mapM (makeStructure types name) options)

makeStructure :: Map TypeRef Type -> String -> (String, [(String, TypeRef)])
              -> Result [Syntax.Declaration]
makeStructure types name (optName, fields) = do
  fields' <- mapMSnd (convertType types) fields
  strMethod <- makeStringMethod optName fields'
  let structDef = Syntax.Structure optName fields'
  let recvType = Syntax.TypeName optName
  let returnStmt = Syntax.Return $ Syntax.IntVal 1 -- TODO: Get actual value
  let methodDef = Syntax.Method ("self", recvType) (enumMethodName name)
                  enumTagMethodDecl (Syntax.Block [returnStmt])
  return [structDef, methodDef, strMethod]

requireFnType :: Map TypeRef Type -> TypeRef -> Result ([TypeRef], TypeRef)
requireFnType types tref = do
  t <- getType types tref
  case t of
   (T.Function ats rt) -> return (ats, rt)
   _                   -> Left $ "compiler bug: expected function type"

makeFnDecl :: Map TypeRef Type -> TypeRef -> [String] -> Result Syntax.FunctionDecl
makeFnDecl types tref argNames = do
  (ats, rts) <- requireFnType types tref
  argTs <- mapM (convertType types) ats
  retT <- convertType types rts
  let args = map (\(t,n) -> Syntax.NamedType n t) (zip argTs argNames)
  return $ Syntax.FunctionDecl args [Syntax.JustType retT]

convertBlock :: Map TypeRef Type -> IR.Statement -> Result Syntax.Statement
convertBlock types stmt = do
  stmts' <- case stmt of
    IR.Block _ stmts -> concatMapM (convertStmt types) stmts
    _                -> convertStmt types stmt
  return $ Syntax.Block stmts'

convertStmt :: Map TypeRef Type -> IR.Statement -> Result [Syntax.Statement]
convertStmt types stmt = case stmt of
  IR.Return Nothing ->
    return [Syntax.JustReturn]
  IR.Return (Just e) -> do
    e' <- convertExpr types e
    return [Syntax.Return e']
  IR.Let s t e -> do
    e' <- convertExpr types e
    t' <- convertType types t
    return [ Syntax.VarStmt s (Just t') e',
             Syntax.VarStmt "_" (Just t') (Syntax.Var s)]
  IR.Assign fs e -> do
    e' <- convertExpr types e
    return [Syntax.Assign [fs] e']
  IR.Block _ _ -> do
    blk <- convertBlock types stmt
    return [blk]
  IR.Expr e -> do
    e' <- convertExpr types e
    return [Syntax.Expr e']
  IR.If e st melse -> do
    e' <- convertExpr types e
    st' <- convertBlock types st
    melse' <- mapM (convertBlock types) melse
    return [Syntax.If Nothing e' st' melse']
  IR.While e st -> do
    e' <- convertExpr types e
    st' <- convertBlock types st
    return [Syntax.For1 e' st']
  IR.Match e cases -> do
    e' <- convertExpr types e
    let varName = "match_var"
    let varStmt = Syntax.VarStmt varName Nothing e'

    cases' <- foldM (\prev c -> fmap Just $ convertMatchCase types varName c prev) Nothing (reverse cases)
    return [Syntax.Block $ varStmt : (toList cases')]

convertMatchCase :: Map TypeRef Type -> String -> IR.MatchCase -> Maybe Syntax.Statement
                 -> Result Syntax.Statement
convertMatchCase types varName (IR.MatchCase matchExpr body) nextCase = do
  (assignments, test) <- convertMatchExpr types varName matchExpr

  body' <- convertStmt types body
  let bodyStmts = case body' of
        [Syntax.Block stmts] -> stmts
        _                    -> body'
  let ifCase = Syntax.Block $ assignments ++ bodyStmts
  let ifStmt = Syntax.If Nothing test ifCase nextCase
  return ifStmt

-- returns (assignments, test)
convertMatchExpr :: Map TypeRef Type -> String -> IR.MatchExpression
                 -> Result ([Syntax.Statement], Syntax.Expression)
convertMatchExpr types varName matchExpr = case matchExpr of
  IR.MatchAnything ->
    return ([], Syntax.BoolVal True)
  IR.MatchVariable name -> do
    let assign = Syntax.VarStmt name Nothing (Syntax.Var varName)
    return ([assign], Syntax.BoolVal True)
  IR.MatchStructure typeRef fields -> do
    test <- createMatchTest types varName matchExpr
    assignments <- gatherAssignments types varName matchExpr
    return (assignments, test)

createMatchTest :: Map TypeRef Type -> String -> IR.MatchExpression -> Result Syntax.Expression
createMatchTest types varName expr = do
  let gatherMatchTests expr path = case expr of
        IR.MatchAnything -> return []
        IR.MatchVariable v -> return []
        IR.MatchStructure t fields -> do
          let fieldNumber = 1  -- TODO!!!
          let typeCall = Syntax.Call (Syntax.FieldAccess path "TODO_ENUM_FUN_NAME") []
          let test = Syntax.Binary Syntax.Eq typeCall (Syntax.IntVal fieldNumber)

          let castValue = Syntax.InterfaceCast (Syntax.GoStruct t) path
          fieldNames <- getFieldNames types t
          fieldTests <- mapM (\(name, field) -> gatherMatchTests field (Syntax.FieldAccess castValue name)) (zip fieldNames fields)
          return $ test : (concat fieldTests)
  tests <- gatherMatchTests expr (Syntax.Var varName)
  return $ foldl1 (Syntax.Binary Syntax.BoolAnd) tests

gatherAssignments :: Map TypeRef Type -> String -> IR.MatchExpression -> Result [Syntax.Statement]
gatherAssignments types varName matchExpr =
  let gather' path matchExpr = case matchExpr of
        IR.MatchAnything ->
          return []
        IR.MatchVariable v ->
          return [Syntax.Let [[v]] path]
        IR.MatchStructure typeRef fields -> do
          let matchedPath = Syntax.InterfaceCast (Syntax.GoStruct typeRef) path
          fieldNames <- getFieldNames types typeRef
          let fieldPaths = map (Syntax.FieldAccess matchedPath) fieldNames
          assignmentLists <- mapM (\(p, f) -> gather' p f) (zip fieldPaths fields)
          return $ concat assignmentLists
      startingPath = Syntax.Var varName
  in gather' startingPath matchExpr

getFieldNames :: Map TypeRef Type -> TypeRef -> Result [String]
getFieldNames types typeRef = case Map.lookup typeRef types of
  Nothing -> fail $ "Compiler bug: can't find type named " ++ show typeRef
  Just t  -> case t of
    T.Struct fields ->
      return $ map fst fields
    _ ->
      fail $ "Compiler bug: can't access fields of non-struct type: " ++ show t

convertValue :: Map TypeRef Type -> IR.Value -> Result Syntax.Expression
convertValue types val = case val of
  IR.StrVal s       -> return $ Syntax.StrVal s
  IR.BoolVal b      -> return $ Syntax.BoolVal b
  IR.IntVal i       -> return $ Syntax.IntVal i
  IR.FloatVal f     -> return $ Syntax.FloatVal f
  IR.StructVal tname fs -> do
    fields <- mapMSnd (convertExpr types) fs
    return $ Syntax.Reference $ Syntax.StructVal tname fields

convertExpr :: Map TypeRef Type -> IR.Expression -> Result Syntax.Expression
convertExpr types expr = case expr of
  IR.Paren e _
    -> do
      e' <- convertExpr types e
      return $ Syntax.Paren e'
  IR.Val val
    -> convertValue types val
  IR.Unary _ op e
    -> do
      e' <- convertExpr types e
      op' <- converUnaryOp op
      return $ Syntax.Unary op' e'
  IR.Binary _ op l r
    -> do
      l' <- convertExpr types l
      r' <- convertExpr types r
      case op of
       E.Power
         -- TODO: Handle taking powers of ints
         -> let powFN = Syntax.FieldAccess (Syntax.Var "math") "Pow"
            in return $ Syntax.Call powFN [l', r']
       _
         -> do
           op' <- convertBinaryOp op
           return $ Syntax.Binary op' l' r'
  IR.Call _ f args
    -> do
      fEx <- convertExpr types f
      argEx <- mapM (convertExpr types) args
      return $ Syntax.Call fEx argEx
  IR.Cast t ex
    -> do
      expr' <- convertExpr types ex
      typ <- convertType types t
      return $ Syntax.TypeCast typ expr'
  IR.Var _ n ->
    if n == "print"
    then return $ Syntax.FieldAccess (Syntax.Var "fmt") "Println"
    else return $ Syntax.Var n
  IR.Access _ e n -> do
    e' <- convertExpr types e
    return $ Syntax.FieldAccess e' n
  IR.Lambda t argNames body -> do
    t' <- convertType types t
    body' <- convertBlock types body
    return $ Syntax.Func t' argNames body'

convertType :: Map TypeRef Type -> TypeRef -> Result Syntax.Type
convertType types tref = do
  t <- getType types tref
  case t of
    T.String
      -> return Syntax.GoString
    T.Float
      -> return Syntax.GoFloat64
    T.Int
      -> return Syntax.GoInt64
    T.Bool
      -> return Syntax.GoBool
    T.Nil
      -> return Syntax.GoVoid -- TODO: come up with a better approach
    T.Function ts t'
      -> do
        argTypes <- mapM (convertType types) ts
        retType <- convertType types t'
        let r = case retType of
              Syntax.GoVoid -> []
              _             -> [retType]
        return $ Syntax.GoFunc argTypes r

    T.Struct _ ->
      return $ Syntax.GoStruct tref
    T.Enum _ ->
      return $ Syntax.GoInterface tref

converUnaryOp :: E.UnaryOp -> Result Syntax.UnaryOp
converUnaryOp E.BitInvert
  = return Syntax.BitInvert
converUnaryOp E.BoolNot
  = return Syntax.BoolNot

convertBinaryOp :: E.BinOp -> Result Syntax.BinaryOp
convertBinaryOp op = case op of
  E.Plus
    -> return Syntax.Plus
  E.Minus
    -> return Syntax.Minus
  E.Times
    -> return Syntax.Times
  E.Divide
    -> return Syntax.Divide
  E.Mod
    -> return Syntax.Mod
  E.BitAnd
    -> return Syntax.BitAnd
  E.BitOr
    -> return Syntax.BitOr
  E.BitXor
    -> return Syntax.BitXor
  E.BoolAnd
    -> return Syntax.BoolAnd
  E.BoolOr
    -> return Syntax.BoolOr
  E.Eq
    -> return Syntax.Eq
  E.NotEq
    -> return Syntax.NotEq
  E.Less
    -> return Syntax.Less
  E.LessEq
    -> return Syntax.LessEq
  E.Greater
    -> return Syntax.Greater
  E.GreaterEq
    -> return Syntax.GreaterEq
  E.LShift
    -> return Syntax.LShift
  E.RShift
    -> return Syntax.RShift
  E.RRShift
    -> return Syntax.RRShift
  _
    -> fail $ show op ++ " should have been already handled"

mapMSnd :: (Monad m) => (a -> m b) -> [(c, a)] -> m [(c, b)]
mapMSnd f = mapM f'
  where f' (c, a) = do
          b <- f a
          return (c, b)

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f list = (liftM concat) (mapM f list)
