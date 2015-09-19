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
type Receiver = [FunctionParameterDecl]

-- STUB
type Literal = String

data QualifiedIdentifier = Qualified Identifier Package

data Declaration
  = ConstDecl [Identifier] (Maybe Type) [Expression]
  | TypeDecl Identifier Type
  | VarDecl [Identifier] (Maybe Type) [Expression]

data TopLevelDecl
  = Declaration Declaration
  | FunctionDecl Identifier FunctionSignature (Maybe [Statement])
  | MethodDecl Receiver Identifier FunctionSignature (Maybe [Statement])

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
  | ChanRead  -- <-

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

data FunctionSignature = Signature [FunctionParameterDecl] (Maybe FunctionResult)

data FunctionParameterDecl = ParameterDecl [Identifier] Type (Maybe Ellipsis)

data FunctionResult
  = FunctionTupleResult [FunctionParameterDecl]
  | FunctionSingleResult Type

data Statement
  = DeclarationStmt Declaration
  | LabeledStmt Identifier Statement
  | SimpleStmt SimpleStmt
  | GoStmt Expression
  | ReturnStmt [Expression]
  | BreakStmt (Maybe Identifier)
  | ContinueStmt (Maybe Identifier)
  | GotoStmt Identifier
  | FallthroughStmt
  | Block [Statement]
  | IfStmt IfStmt
  | SwitchStmt SwitchStmt
  | SelectStmt [CommClauseStmt]
  | ForStmt ForStmt
  | DeferStmt Expression

data SimpleStmt
  = Empty
  | ExpressionStmt Expression
  | SendStmt SendStmt
  | IncStmt Expression
  | DecStmt Expression
  | AssignmentStmt [Expression] AssignOp [Expression]
  | ShortVarDeclStmt [Identifier] [Expression]

data IfStmt = If (Maybe SimpleStmt) Expression [Statement] (Maybe ElseStmt)

data ElseStmt
  = ElseIf IfStmt
  | Else [Statement]

data SwitchStmt
  = ExprSwitchStmt (Maybe SimpleStmt) (Maybe Expression) [ExprCaseClauseStmt]
  | TypeSwitchStmt (Maybe SimpleStmt) TypeSwitchGuardStmt [TypeCaseClauseStmt]

data ExprCaseClauseStmt
  = ExprCaseClause [Expression] [Statement]
  | DefaultCaseClause [Statement]

data TypeSwitchGuardStmt = TypeSwitchGuard (Maybe Identifier) PrimaryExpr

data TypeCaseClauseStmt
  = TypeCaseClause [Type] [Statement]
  | DefaultTypeCaseClause [Statement]

data CommClauseStmt = CommClause CommCaseStmt [Statement]

data CommCaseStmt
  = SendCommClause SendStmt
  | RecvCommClause RecvStmt
  | DefaultCommClause

data SendStmt = Send Expression Expression

data RecvStmt = Recv RecvTarget Expression

data RecvTarget
  = RecvAssignment [Expression]
  | RecvShortVarDecl [Identifier]

data ForStmt = For ForDefinition [Statement]

data ForDefinition
  = Condition Expression
  | ForClause (Maybe SimpleStmt) (Maybe Expression) (Maybe SimpleStmt)
  | RangeClause RangeTarget Expression

data RangeTarget
  = RangeAssignment [Expression]
  | RangeShortVarDecl [Identifier]

data AssignOp
  = AssignAddOp AddOp
  | AssignMulOp MulOp
  | Assign
