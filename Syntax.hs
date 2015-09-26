module Syntax where

import qualified Data.Complex as ComplexData
import qualified Data.Ord as OrdData

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
type ImportDecl = [ImportSpec]

data QualifiedIdentifier = Qualified Package Identifier
  deriving (Eq, Ord, Show)

data Declaration
  = ConstDecl [Identifier] (Maybe Type) [Expression]
  | TypeDecl Identifier Type
  | VarDecl [Identifier] (Maybe Type) [Expression]
  deriving (Eq, Ord, Show)

data TopLevelDecl
  = Declaration Declaration
  | FunctionDecl Identifier FunctionSignature (Maybe [Statement])
  | ExternFunctionDecl Identifier FunctionSignature    -- this is not part of go RFC
  | MethodDecl Receiver Identifier FunctionSignature (Maybe [Statement])
  deriving (Eq, Ord, Show)

data Expression
  = Unary UnaryExpr
  | Binary BinaryOp Expression Expression
  deriving (Eq, Ord, Show)

data UnaryExpr
  = PrimaryExpr PrimaryExpr
  -- FIXME: had to change this from 'UnaryExpr' to 'Expression'
  -- don't know if it will cause any problems
  | UnaryOpExpr UnaryOp Expression
  deriving (Eq, Ord, Show)

data PrimaryExpr
  = Operand Operand
  | Conversion Conversion
  | Selector PrimaryExpr Identifier
  | Index PrimaryExpr Expression
  | Slice PrimaryExpr (Maybe SliceStart) (Maybe SliceEnd) (Maybe SliceStep)
  | TypeAssertion PrimaryExpr Type
  | Arguments PrimaryExpr [Expression] (Maybe Type) (Maybe Ellipsis)
  deriving (Eq, Ord, Show)

data BinaryOp
  = Or        -- ||
  | And       -- &&
  | RelBinOp RelOp
  | AddBinOp AddOp
  | MulBinOp MulOp
  deriving (Eq, Ord, Show)

data RelOp
  = Equals    -- ==
  | NotEquals -- !=
  | Less      -- <
  | LessEq    -- <=
  | Greater   -- >
  | GreaterEq -- >=
  deriving (Eq, Ord, Show)

data AddOp
  = Plus      -- +
  | Minus     -- -
  | BitOr     -- |
  | BitXor    -- ^
  deriving (Eq, Ord, Show)

data MulOp
  = Mult      -- *
  | Div       -- /
  | Mod       -- %
  | LShift    -- <<
  | RShift    -- >>
  | BitAnd    -- &
  | AndXor    -- &^ - FIXME: no idea what is that
  deriving (Eq, Ord, Show)

data UnaryOp
  = UnPlus    -- +
  | UnMinus   -- -
  | Not       -- !
  | UnCaret   -- ^
  | Deref     -- *
  | Pointerof -- &
  | ChanRead  -- <-
  deriving (Eq, Ord, Show)

data TypeName
  = Identifier Identifier
  | QualifiedIdentifier QualifiedIdentifier
  deriving (Eq, Ord, Show)

data TypeWithEllipsis = TypeWithEllipsis Type (Maybe Ellipsis)

data Operand
  = Literal Literal
  | OperandName OperandName
  | MethodExpr Type Identifier
  | ExpressionInParens Expression
  deriving (Eq, Ord, Show)

data OperandName
  = OperandIdentifier Identifier
  | OperandQualified QualifiedIdentifier
  deriving (Eq, Ord, Show)

data Conversion = TypeConversion Type Expression
  deriving (Eq, Ord, Show)

data Type
  = TypeName TypeName
  | TypeLiteral TypeLiteral
  deriving (Eq, Ord, Show)

data TypeLiteral
  = ArrayType ArrayLength Type
  | StructType [FieldDecl]
  | PointerType Type
  | FunctionType FunctionSignature
  | InterfaceType [MethodSpec]
  | SliceType Type
  | MapType Type Type
  | ChannelType Type (Maybe ReadChannel) (Maybe WriteChannel)
  deriving (Eq, Ord, Show)

data FieldDecl
  = Field [Identifier] Type
  | AnonymousField TypeName
  deriving (Eq, Ord, Show)

data MethodSpec
  = Method Identifier FunctionSignature
  | Interface TypeName
  deriving (Eq, Ord, Show)

data FunctionSignature = Signature [FunctionParameterDecl] (Maybe FunctionResult)
  deriving (Eq, Ord, Show)

data FunctionParameterDecl = ParameterDecl [Identifier] Type (Maybe Ellipsis)
  deriving (Eq, Ord, Show)

data FunctionResult
  = FunctionTupleResult [FunctionParameterDecl]
  | FunctionSingleResult Type
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

data SimpleStmt
  = Empty
  | ExpressionStmt Expression
  | SendStmt SendStmt
  | IncStmt Expression
  | DecStmt Expression
  | AssignmentStmt [Expression] AssignOp [Expression]
  | ShortVarDeclStmt [Identifier] [Expression]
  deriving (Eq, Ord, Show)

data IfStmt = If (Maybe SimpleStmt) Expression [Statement] (Maybe ElseStmt)
  deriving (Eq, Ord, Show)

data ElseStmt
  = ElseIf IfStmt
  | Else [Statement]
  deriving (Eq, Ord, Show)

data SwitchStmt
  = ExprSwitchStmt (Maybe SimpleStmt) (Maybe Expression) [ExprCaseClauseStmt]
  | TypeSwitchStmt (Maybe SimpleStmt) TypeSwitchGuardStmt [TypeCaseClauseStmt]
  deriving (Eq, Ord, Show)

data ExprCaseClauseStmt
  = ExprCaseClause [Expression] [Statement]
  | DefaultCaseClause [Statement]
  deriving (Eq, Ord, Show)

data TypeSwitchGuardStmt = TypeSwitchGuard (Maybe Identifier) PrimaryExpr
  deriving (Eq, Ord, Show)

data TypeCaseClauseStmt
  = TypeCaseClause [Type] [Statement]
  | DefaultTypeCaseClause [Statement]
  deriving (Eq, Ord, Show)

data CommClauseStmt = CommClause CommCaseStmt [Statement]
  deriving (Eq, Ord, Show)

data CommCaseStmt
  = SendCommClause SendStmt
  | RecvCommClause RecvStmt
  | DefaultCommClause
  deriving (Eq, Ord, Show)

data SendStmt = Send Expression Expression
  deriving (Eq, Ord, Show)

data RecvStmt = Recv RecvTarget Expression
  deriving (Eq, Ord, Show)

data RecvTarget
  = RecvAssignment [Expression]
  | RecvShortVarDecl [Identifier]
  deriving (Eq, Ord, Show)

data ForStmt = For ForDefinition [Statement]
  deriving (Eq, Ord, Show)

data ForDefinition
  = Condition Expression
  | ForClause (Maybe SimpleStmt) (Maybe Expression) (Maybe SimpleStmt)
  | RangeClause RangeTarget Expression
  deriving (Eq, Ord, Show)

data RangeTarget
  = RangeAssignment [Expression]
  | RangeShortVarDecl [Identifier]
  deriving (Eq, Ord, Show)

data AssignOp
  = AssignAddOp AddOp
  | AssignMulOp MulOp
  | Assign
  deriving (Eq, Ord, Show)

data Literal
  = IntLiteral Integer
  | FloatLiteral Double
  | ImaginaryLiteral ComplexValue
  | RuneLiteral Char
  | StringLiteral String
  | CompositeLiteral LiteralType LiteralValue
  | FunctionLiteral FunctionSignature [Statement]
  deriving (Eq, Ord, Show)

data LiteralType
  = StructTypeLiteral [FieldDecl]
  | ArrayTypeLiteral ArrayLength Type
  | AutoSizeArrayTypeLiteral Type
  | SliceTypeLiteral Type
  | MapTypeLiteral Type Type
  | TypeNameLiteral TypeName
  deriving (Eq, Ord, Show)

data LiteralValue = LiteralValue [LiteralElement]
  deriving (Eq, Ord, Show)

data LiteralElement = LiteralElement (Maybe LiteralKey) Value
  deriving (Eq, Ord, Show)

data LiteralKey
  = FieldName Identifier
  | KeyExpr Expression
  | KeyLiteralValue LiteralValue
  deriving (Eq, Ord, Show)

data Value
  = ValueExpr Expression
  | ValueLiteralValue LiteralValue
  deriving (Eq, Ord, Show)

data SourceFile = SourceFile Identifier [ImportDecl] [TopLevelDecl]
  deriving (Eq, Ord, Show)

data ImportSpec = ImportSpec Identifier String
  deriving (Eq, Ord, Show)

type ComplexValue = ComplexData.Complex Double

instance (Eq t, Ord t, RealFloat t) => Ord (ComplexData.Complex t) where
  compare a b =
    case byRealPart of
      OrdData.EQ -> byImagPart
      _ -> byRealPart
    where
      byRealPart = compare (ComplexData.realPart a) (ComplexData.realPart b)
      byImagPart = compare (ComplexData.imagPart a) (ComplexData.imagPart b)
