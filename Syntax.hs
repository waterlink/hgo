module Syntax where

type Identifier = String
type Package = Identifier
type SliceStart = Integer
type SliceEnd = Integer
type SliceStep = Integer
type Ellipsis = ()
type ArrayLength = Integer
type ReadChannel = ()
type WriteChannel = ()

-- STUB
type FunctionSignature = String
type FunctionBody = String
type Receiver = String
type Literal = String

data QualifiedIdentifier = Qualified Identifier Package

data Declaration
  = ConstDecl [Identifier] (Maybe Type) [Expression]
  | TypeDecl Identifier Type
  | VarDecl [Identifier] (Maybe Type) [Expression]

data TopLevelDecl
  = Declaration Declaration
  | FunctionDecl Identifier FunctionSignature (Maybe FunctionBody)
  | MethodDecl Receiver Identifier FunctionSignature (Maybe FunctionBody)

data Expression
  = Unary UnaryExpr
  | Binary BinaryOp Expression Expression

data UnaryExpr
  = PrimaryExpr PrimaryExpr
  | UnaryOp UnaryOp UnaryExpr

data PrimaryExpr
  = Operand Operand
  | Conversion Conversion
  | Selector PrimaryExpr Identifier
  | Index PrimaryExpr Expression
  | Slice PrimaryExpr (Maybe SliceStart) (Maybe SliceEnd) (Maybe SliceStep)
  | TypeAssertion PrimaryExpr Type
  | Arguments PrimaryExpr [Expression] (Maybe Type) (Maybe Ellipsis)

data BinaryOp
  = Or        -- ||
  | And       -- &&
  | RelOp
  | AddOp
  | MulOp

data RelOp
  = Equals    -- ==
  | NotEquals -- !=
  | Less      -- <
  | LessEq    -- <=
  | Greater   -- >
  | GreaterEq -- >=

data AddOp
  = Plus      -- +
  | Minus     -- -
  | BitOr     -- |
  | BitXor    -- ^

data MulOp
  = Mult      -- *
  | Div       -- /
  | Mod       -- %
  | LShift    -- <<
  | RShift    -- >>
  | BitAnd    -- &
  | AndXor    -- &^ - FIXME: no idea what is that

data UnaryOp
  = UnPlus    -- +
  | UnMinus   -- -
  | Not       -- !
  | UnCaret   -- ^
  | Deref     -- *
  | Pointerof -- &
  | CharRead  -- <-

data TypeName
  = Identifier Identifier
  | QualifiedIdentifier QualifiedIdentifier

data Operand
  = Literal Literal
  | OperandType TypeName
  | MethodExpr Type Identifier
  | Expression Expression

data Conversion = TypeConversion Type Expression

data Type
  = TypeName TypeName
  | TypeLiteral TypeLiteral

data TypeLiteral
  = ArrayType (Maybe ArrayLength) Type
  | StructType [FieldDecl]
  | PointerType Type
  | FunctionType FunctionSignature
  | InterfaceType [MethodSpec]
  | SliceType Type
  | MapType Type Type
  | ChannelType Type (Maybe ReadChannel) (Maybe WriteChannel)

data FieldDecl
  = Field [Identifier] Type
  | AnonymousField TypeName

data MethodSpec
  = Method Identifier FunctionSignature
  | Interface TypeName
