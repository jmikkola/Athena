module Backends.Go.Syntax where

data Type
  = GoInt64
  | GoInt
  | GoFloat64
  | GoBool
  | GoString
  | GoChar
  | GoFunc { argTypes :: [Type]
           , retTypes :: [Type] }
  | GoStruct String
  | GoInterface String
  | GoVoid -- a bit of a hack
  | TypeName String
  deriving (Eq, Show)

data FunctionDecl
  = FunctionDecl [NamedT] [NamedT]
  deriving (Eq, Show)

data NamedT
  = JustName String
  | JustType Type
  | NamedType String Type
  deriving (Eq, Show)

data Declaration
  = Structure String [(String, Type)]
  | Interface String [(String, FunctionDecl)]
  | Variable String (Maybe Type) Expression
  | Constant String Expression
  | Method (String, Type) String FunctionDecl Statement
  | Function String FunctionDecl Statement
  deriving (Eq, Show)

data Statement
  = JustReturn
  | Return Expression
  | VarStmt String (Maybe Type) Expression
  | Assign [String] Expression
  | Expr Expression
  | If Expression Statement (Maybe Statement)
  | Loop Statement
  | For1 Expression Statement
  | For3 (Maybe Statement) Expression (Maybe Statement) Statement
  | Block [Statement]
  deriving (Eq, Show)

data Expression
  = Paren Expression
  | Unary UnaryOp Expression
  | Binary BinaryOp Expression Expression
  | Call Expression [Expression]
  | InterfaceCast Type Expression
  | TypeCast Type Expression
  | Var String
  | FieldAccess Expression String
  | ArrayAccess Expression Expression
  | Func Type [String] Statement
  | StrVal String
  | BoolVal Bool
  | IntVal Int
  | FloatVal Float
  | StructVal String [(String, Expression)]
  | Reference Expression
  | Dereference Expression
  deriving (Show, Eq)

data UnaryOp
  = BitInvert
  | BoolNot
  deriving (Eq, Show)

data BinaryOp
  = Plus
  | Minus
  | Times
  | Divide
  | Mod
  | BitAnd
  | BitOr
  | BitXor
  | BoolAnd
  | BoolOr
  | Eq
  | NotEq
  | Less
  | LessEq
  | Greater
  | GreaterEq
  | LShift
  | RShift
  | RRShift
  deriving (Eq, Show)
