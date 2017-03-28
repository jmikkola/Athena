module Backends.Interpreter.Values where

data Value
  = BoolVal Bool
  | IntVal Int
  | FloatVal Float
  | StringVal String
  | FunctionVal Type 
  | StructVal Type [Value]

getType :: Value -> Type
getType BoolVal _ = "Bool"
