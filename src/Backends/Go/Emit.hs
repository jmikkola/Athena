module Backends.Go.Emit where

import Control.Monad.State

--import qualified Backends.Go.Syntax as Syntax
import Backends.Go.Syntax (
  Type (..),
  FunctionDecl (..),
  NamedT (..),
  Declaration (..),
  Statement (..),
  Expression (..),
  UnaryOp (..),
  BinaryOp (..) )

data Output = Output { indent :: Int
                     , pieces :: [String]
                     }
type Result = Either String
type EmitState = StateT Output Result

write :: String -> EmitState ()
write s = modify (\o -> o { pieces = (s : pieces o) })

getIndent :: EmitState Int
getIndent = do
  o <- get
  return $ indent o

alterIndent :: Int -> EmitState ()
alterIndent delta = modify (\o -> o { indent = delta + indent o })

increaseIndent :: EmitState ()
increaseIndent = alterIndent 1

decreateIndent :: EmitState ()
decreateIndent = alterIndent (-1)

writeIndent :: EmitState ()
writeIndent = do
  n <- getIndent
  write $ indentString n

indentString :: Int -> String
indentString n = replicate n '\t'

--- Actual Emitters ---

emitType :: Type -> EmitState ()
emitType t = case t of
  GoInt64
    -> write "int64"
  GoInt
    -> write "int"
  GoFloat64
    -> write "float64"
  GoBool
    -> write "bool"
  GoString
    -> write "string"
  GoChar
    -> write "rune"
  GoFunc args rets
    -> do
      write "func ("
      intersperse (write ", ") (map emitType args)
      write ")"
      case rets of
       []
         -> return ()
       [GoVoid]
         -> return ()
       [t']
         -> do
           write " "
           emitType t'
       _
         -> do
           write " ("
           intersperse (write ", ") (map emitType rets)
           write ")"
  GoStruct name
    -> do
      write "*"
      write name
  GoInterface name
    -> write name
  GoVoid
    -> return ()
  TypeName name
    -> write name

emitFunctionDecl :: FunctionDecl -> EmitState ()
emitFunctionDecl (FunctionDecl args rets) = do
  write "func ("
  intersperse (write ", ") (map emitNamedT args)
  write ")"
  writeReturn rets

writeReturn :: [NamedT] -> EmitState ()
writeReturn []
  = return ()
writeReturn [JustType GoVoid]
  = return ()
writeReturn typs
  = let inner = intersperse (write ", ") (map emitNamedT typs)
    in if retNeedsParens typs
       then do
         write " ("
         inner
         write ")"
       else do
         write " "
         inner
         return ()

retNeedsParens :: [NamedT] -> Bool
retNeedsParens []
  = False
retNeedsParens [NamedType _ _]
  = True
retNeedsParens [_]
  = False
retNeedsParens _
  = True

emitNamedT :: NamedT -> EmitState ()
emitNamedT (JustName name)
  = write name
emitNamedT (JustType t)
  = emitType t
emitNamedT (NamedType name t)
  = do
   write name
   write " "
   emitType t

emitExpression :: Expression -> EmitState ()
emitExpression expr = case expr of
  Paren e
    -> do
      write "("
      emitExpression e
      write ")"
  Unary op e
    -> do
      emitUnaryOp op
      emitExpression e
  Binary _ _ _
    -> undefined -- TODO
  Call _ _
    -> undefined -- TODO
  InterfaceCast _ _
    -> undefined -- TODO
  TypeCast _ _
    -> undefined -- TODO
  Var name
    -> write name
  FieldAccess _ _
    -> undefined -- TODO
  ArrayAccess _ _
    -> undefined -- TODO
  Func _ _ _
    -> undefined -- TODO
  StrVal _
    -> undefined -- TODO
  BoolVal _
    -> undefined -- TODO
  IntVal _
    -> undefined -- TODO
  FloatVal _
    -> undefined -- TODO
  StructVal _ _
    -> undefined -- TODO

emitUnaryOp :: UnaryOp -> EmitState ()
emitUnaryOp BitInvert
  = write "^"
emitUnaryOp BoolNot
  = write "!"

intersperse :: EmitState () -> [EmitState ()] -> EmitState ()
intersperse sep lst =
  case lst of
   (a1:a2:rest) -> do
     a1
     sep
     intersperse sep (a2:rest)
   [a]          -> a
   []           -> return ()
