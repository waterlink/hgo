module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import qualified Lexer as L
import qualified Syntax as S

newUnaryOp :: S.UnaryOp -> S.Expression -> S.Expression
newUnaryOp f e = S.Unary (S.UnaryOpExpr f e)

unary s f = Ex.Prefix (L.operator s >> return (newUnaryOp f))
binary s f assoc = Ex.Infix (L.operator s >> return (S.Binary f)) assoc
binaryMul s f assoc = Ex.Infix (L.operator s >> return (S.Binary $ S.MulBinOp f)) assoc
binaryAdd s f assoc = Ex.Infix (L.operator s >> return (S.Binary $ S.AddBinOp f)) assoc
binaryRel s f assoc = Ex.Infix (L.operator s >> return (S.Binary $ S.RelBinOp f)) assoc

opTable = [[unary "+" S.UnPlus,
            unary "-" S.UnMinus,
            unary "!" S.Not,
            unary "^" S.UnCaret,
            unary "*" S.Deref,
            unary "&" S.Pointerof,
            unary "<-" S.ChanRead],

           [binaryMul "*" S.Mult Ex.AssocLeft,
            binaryMul "/" S.Div Ex.AssocLeft,
            binaryMul "%" S.Mod Ex.AssocLeft,
            binaryMul "<<" S.LShift Ex.AssocLeft,
            binaryMul ">>" S.RShift Ex.AssocLeft,
            binaryMul "&" S.BitAnd Ex.AssocLeft,
            binaryMul "&^" S.AndXor Ex.AssocLeft],

           [binaryAdd "+" S.Plus Ex.AssocLeft,
            binaryAdd "-" S.Minus Ex.AssocLeft,
            binaryAdd "|" S.BitOr Ex.AssocLeft,
            binaryAdd "^" S.BitXor Ex.AssocLeft],

           [binaryRel "==" S.Equals Ex.AssocLeft,
            binaryRel "!=" S.NotEquals Ex.AssocLeft,
            binaryRel "<" S.Less Ex.AssocLeft,
            binaryRel "<=" S.LessEq Ex.AssocLeft,
            binaryRel ">" S.Greater Ex.AssocLeft,
            binaryRel ">=" S.GreaterEq Ex.AssocLeft],

           [binary "&&" S.And Ex.AssocLeft],

           [binary "||" S.Or Ex.AssocLeft]]

qualifiedIdentifier :: Parser S.QualifiedIdentifier
qualifiedIdentifier = do
  package <- L.identifier
  L.operator "."
  name <- L.identifier
  return $ S.Qualified package name

typeName :: Parser S.TypeName
typeName = qualified <|> identifier
  where
    identifier = try $ do
      name <- L.identifier
      return $ S.Identifier name
    qualified = try $ do
      name <- qualifiedIdentifier
      return $ S.QualifiedIdentifier name

typeWithEllipsis :: Parser S.TypeWithEllipsis
typeWithEllipsis = withEllipsis <|> without
  where
    withEllipsis = try $ do
      L.operator "..."
      value <- fullTypeValue
      return $ S.TypeWithEllipsis value (Just ())
    without = try $ do
      value <- fullTypeValue
      return $ S.TypeWithEllipsis value Nothing

fullTypeValue :: Parser S.Type
fullTypeValue = name <|> literal
  where
    name = do
      value <- typeName
      return $ S.TypeName value
    literal = do
      value <- typeLiteral
      return $ S.TypeLiteral value

-- FIXME: add support for:
-- - struct
literalType :: Parser S.LiteralType
literalType
    = array
  <|> autoarray
  <|> slice
  <|> map
  <|> name
  where
    array :: Parser S.LiteralType
    array = try $ do
      size <- L.brackets L.integer
      value <- fullTypeValue
      return $ S.ArrayTypeLiteral size value 

    autoarray :: Parser S.LiteralType
    autoarray = try $ do
      L.brackets $ L.operator "..."
      value <- fullTypeValue
      return $ S.AutoSizeArrayTypeLiteral value 

    slice :: Parser S.LiteralType
    slice = try $ do
      L.operator "["
      L.operator "]"
      value <- fullTypeValue
      return $ S.SliceTypeLiteral value

    map :: Parser S.LiteralType
    map = try $ do
      L.keyword "map"
      key <- L.brackets fullTypeValue
      value <- fullTypeValue
      return $ S.MapTypeLiteral key value

    name :: Parser S.LiteralType
    name = try $ do
      value <- typeName
      return $ S.TypeNameLiteral value

-- FIXME: add support for:
-- - struct
-- - interface
-- - function
typeLiteral :: Parser S.TypeLiteral
typeLiteral
    = array
  <|> pointer
  <|> slice
  <|> map
  <|> channelro
  <|> channelwo
  <|> channelrw
  where
    -- FIXME: support expressions inside of [ ... ]
    array = try $ do
      size <- L.brackets L.integer
      value <- fullTypeValue
      return $ S.ArrayType size value 

    pointer = try $ do
      L.operator "*"
      value <- fullTypeValue
      return $ S.PointerType value

    slice = try $ do
      L.operator "["
      L.operator "]"
      value <- fullTypeValue
      return $ S.SliceType value

    map = try $ do
      L.keyword "map"
      key <- L.brackets fullTypeValue
      value <- fullTypeValue
      return $ S.MapType key value

    channelro = try $ do
      L.operator "<-"
      L.keyword "chan"
      value <- fullTypeValue
      return $ S.ChannelType value (Just ()) Nothing

    channelwo = try $ do
      L.keyword "chan"
      L.operator "<-"
      value <- fullTypeValue
      return $ S.ChannelType value Nothing (Just ())

    channelrw = try $ do
      L.keyword "chan"
      value <- fullTypeValue
      return $ S.ChannelType value (Just ()) (Just ())

paramdecl :: Parser S.FunctionParameterDecl
paramdecl = withName <|> without
  where
    withName = try $ do
      names <- L.commaSep L.identifier
      S.TypeWithEllipsis typeValue ellipsis <- typeWithEllipsis
      return $ S.ParameterDecl names typeValue ellipsis
    without = try $ do
      S.TypeWithEllipsis typeValue ellipsis <- typeWithEllipsis
      return $ S.ParameterDecl [] typeValue ellipsis

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace L.l
  r <- p
  eof
  return r

paramdecllist :: Parser [S.FunctionParameterDecl]
paramdecllist = try (L.parens $ L.commaSep paramdecl)

resultType :: Parser (Maybe S.FunctionResult)
resultType = tuple <|> single <|> nothing
  where
    tuple = do
      values <- paramdecllist
      return $ Just (S.FunctionTupleResult values)
    single = do
      value <- fullTypeValue
      return $ Just (S.FunctionSingleResult value)
    nothing = return Nothing

addOp :: Parser S.AddOp
addOp = plus <|> minus <|> or <|> xor
  where
    plus = try $ do
      L.operator "+"
      return S.Plus
    minus = try $ do
      L.operator "-"
      return S.Minus
    or = try $ do
      L.operator "|"
      return S.BitOr
    xor = try $ do
      L.operator "^"
      return S.BitXor

mulOp :: Parser S.MulOp
mulOp = mult <|> div <|> mod <|> lshift <|> rshift <|> and <|> andxor
  where
    mult :: Parser S.MulOp
    mult = try $ do
      L.operator "*"
      return S.Mult

    div :: Parser S.MulOp
    div = try $ do
      L.operator "/"
      return S.Div

    mod :: Parser S.MulOp
    mod = try $ do
      L.operator "%"
      return S.Mod

    lshift :: Parser S.MulOp
    lshift = try $ do
      L.operator "<<"
      return S.LShift

    rshift :: Parser S.MulOp
    rshift = try $ do
      L.operator ">>"
      return S.RShift

    and :: Parser S.MulOp
    and = try $ do
      L.operator "&"
      return S.BitAnd

    andxor :: Parser S.MulOp
    andxor = try $ do
      L.operator "&^"
      return S.AndXor

assignOp :: Parser S.AssignOp
assignOp = add <|> mul <|> assign
  where
    add = try $ do
      op <- addOp
      return $ S.AssignAddOp op
    mul = try $ do
      op <- mulOp
      return $ S.AssignMulOp op
    assign = try $ do
      L.operator "="
      return S.Assign

sendstmt :: Parser S.SendStmt
sendstmt = try $ do
  left <- expression
  L.operator "<-"
  right <- expression
  return $ S.Send left right

simplestmt :: Parser S.SimpleStmt
simplestmt
    = send
  <|> incstmt
  <|> decstmt
  <|> assignstmt
  <|> shortvardecl
  <|> exprstmt
  <|> emptystmt
  where
    exprstmt = try $ do
      value <- expression
      return $ S.ExpressionStmt value
    send = try $ do
      value <- sendstmt
      return $ S.SendStmt value
    incstmt = try $ do
      value <- expression
      L.operator "++"
      return $ S.IncStmt value
    decstmt = try $ do
      value <- expression
      L.operator "--"
      return $ S.DecStmt value
    assignstmt = try $ do
      left <- L.commaSep expression
      op <- assignOp
      right <- L.commaSep expression
      return $ S.AssignmentStmt left op right
    shortvardecl = try $ do
      left <- L.commaSep L.identifier
      L.operator ":="
      right <- L.commaSep expression
      return $ S.ShortVarDeclStmt left right
    emptystmt = return S.Empty

-- FIXME: add support for:
-- - declaration
-- - labeled stmt
-- - goto stmt
-- - if stmt
-- - switch stmt
-- - select stmt
-- - for stmt
statement :: Parser S.Statement
statement
    = gostmt
  <|> returnstmt
  <|> breakstmtWithLabel
  <|> breakstmt
  <|> continuestmtWithLabel
  <|> continuestmt
  <|> fallthroughstmt
  <|> blockstmt
  <|> deferstmt
  <|> simple
  where
    gostmt = try $ do
      L.keyword "go"
      value <- expression
      return $ S.GoStmt value
    returnstmt = try $ do
      L.keyword "return"
      values <- L.commaSep expression
      return $ S.ReturnStmt values
    breakstmt = try $ do
      L.keyword "break"
      return $ S.BreakStmt Nothing
    breakstmtWithLabel = try $ do
      L.keyword "break"
      value <- L.identifier
      return $ S.BreakStmt (Just value)
    continuestmt = try $ do
      L.keyword "continue"
      return $ S.ContinueStmt Nothing
    continuestmtWithLabel = try $ do
      L.keyword "continue"
      value <- L.identifier
      return $ S.ContinueStmt (Just value)
    fallthroughstmt = try $ do
      L.keyword "fallthrough"
      return $ S.FallthroughStmt
    blockstmt = try $ do
      value <- block
      return $ S.Block value
    deferstmt = try $ do
      L.keyword "defer"
      value <- expression
      return $ S.DeferStmt value
    simple = try $ do
      value <- simplestmt
      return $ S.SimpleStmt value

expression :: Parser S.Expression
expression = Ex.buildExpressionParser opTable exprFactor

exprFactor :: Parser S.Expression
exprFactor = try $ do
  value <- primaryExpr
  return $ S.Unary (S.PrimaryExpr value)

primaryExpr :: Parser S.PrimaryExpr
primaryExpr = conversion <|> operandExpr
  where
    afterPrimaryExpr :: S.PrimaryExpr -> Parser S.PrimaryExpr
    afterPrimaryExpr value = option value (
                            selector value
                        <|> index value
                        <|> slice value
                        <|> sliceWithStep value
                        <|> typeAssertion value
                        <|> arguments value)

    operandExpr :: Parser S.PrimaryExpr
    operandExpr = try $ do
      operandValue <- operand
      afterPrimaryExpr $ S.Operand operandValue

    conversion :: Parser S.PrimaryExpr
    conversion = try $ do
      typeValue <- fullTypeValue
      L.operator "("
      innerValue <- expression
      optional $ L.operator ","
      L.operator ")"
      afterPrimaryExpr $ S.Conversion (S.TypeConversion typeValue innerValue)

    selector :: S.PrimaryExpr -> Parser S.PrimaryExpr
    selector value = try $ do
      L.operator "."
      name <- L.identifier
      afterPrimaryExpr $ S.Selector value name

    index :: S.PrimaryExpr -> Parser S.PrimaryExpr
    index value = try $ do
      indexValue <- L.brackets expression
      afterPrimaryExpr $ S.Index value indexValue

    slice :: S.PrimaryExpr -> Parser S.PrimaryExpr
    slice value = try $ do
      L.operator "["
      start <- optionMaybe L.integer
      L.operator ":"
      end <- optionMaybe L.integer
      L.operator "]"
      afterPrimaryExpr $ S.Slice value start end Nothing

    sliceWithStep :: S.PrimaryExpr -> Parser S.PrimaryExpr
    sliceWithStep value = try $ do
      L.operator "["
      start <- optionMaybe L.integer
      L.operator ":"
      end <- L.integer
      L.operator ":"
      step <- L.integer
      afterPrimaryExpr $ S.Slice value start (Just end) (Just step)

    typeAssertion :: S.PrimaryExpr -> Parser S.PrimaryExpr
    typeAssertion value = try $ do
      L.operator "."
      typeValue <- L.parens fullTypeValue
      afterPrimaryExpr $ S.TypeAssertion value typeValue

    -- FIXME:
    -- - figure out what Type can do here (see go's RFC)
    -- - add support for Ellipsis
    arguments :: S.PrimaryExpr -> Parser S.PrimaryExpr
    arguments value = try $ do
      args <- L.parens $ L.commaSep expression
      afterPrimaryExpr $ S.Arguments value args Nothing Nothing

operand :: Parser S.Operand
operand = literal <|> name <|> qualifiedName <|> methodExpr <|> exprInParens
  where
    literal :: Parser S.Operand
    literal = try $ do
      value <- literalExpr
      return $ S.Literal value

    name :: Parser S.Operand
    name = try $ do
      value <- L.identifier
      return $ S.OperandName (S.OperandIdentifier value)

    qualifiedName :: Parser S.Operand
    qualifiedName = try $ do
      value <- qualifiedIdentifier
      return $ S.OperandName (S.OperandQualified value)

    -- FIXME: add support for "(" ReceiverType ")"
    -- FIXME: currently allows all type literals, but shouldn't ?
    methodExpr :: Parser S.Operand
    methodExpr = try $ do
      typeValue <- fullTypeValue
      L.operator "."
      name <- L.identifier
      return $ S.MethodExpr typeValue name

    exprInParens :: Parser S.Operand
    exprInParens = try $ do
      value <- L.parens expression
      return $ S.ExpressionInParens value

-- FIXME: add support for function literal
literalExpr :: Parser S.Literal
literalExpr = basic <|> composite
  where
    integer :: Parser S.Literal
    integer = try $ do
      value <- L.integer
      return $ S.IntLiteral value

    float :: Parser S.Literal
    float = try $ do
      value <- L.float
      return $ S.FloatLiteral value

    imaginary :: Parser S.Literal
    imaginary = try $ do
      value <- L.imaginary
      return $ S.ImaginaryLiteral value

    rune :: Parser S.Literal
    rune = try $ do
      value <- L.rune
      return $ S.RuneLiteral value

    string :: Parser S.Literal
    string = try $ do
      value <- L.string
      return $ S.StringLiteral value

    basic :: Parser S.Literal
    basic = float
        <|> imaginary
        <|> integer
        <|> rune
        <|> string

    composite :: Parser S.Literal
    composite = try $ do
      typeValue <- literalType
      value <- literalValue
      return $ S.CompositeLiteral typeValue value

literalValue :: Parser S.LiteralValue
literalValue = try $ do
  elements <- L.braces $ L.commaSep element
  return $ S.LiteralValue elements
  where
    element = try $ do
      key <- option Nothing withKey
      value <- elementValue <|> elementExpr
      return $ S.LiteralElement key value

    elementValue = try $ do
      value <- literalValue
      return $ S.ValueLiteralValue value

    elementExpr = try $ do
      value <- expression
      return $ S.ValueExpr value

    withKey = fieldKey <|> literalKey <|> exprKey

    fieldKey = try $ do
      key <- L.identifier
      L.operator ":"
      return $ Just (S.FieldName key)

    literalKey = try $ do
      key <- literalValue
      L.operator ":"
      return $ Just (S.KeyLiteralValue key)

    exprKey = try $ do
      key <- expression
      L.operator ":"
      return $ Just (S.KeyExpr key)

block :: Parser [S.Statement]
block = L.braces $ statement `sepBy` L.semi

topfundecl :: Parser S.TopLevelDecl
topfundecl = do
  L.keyword "func"
  name <- L.identifier
  args <- paramdecllist
  result <- resultType
  body <- optionMaybe block
  return $ S.FunctionDecl name (S.Signature args result) body

topexterndecl :: Parser S.TopLevelDecl
topexterndecl = do
  L.keyword "extern"
  name <- L.identifier
  args <- paramdecllist
  result <- resultType
  return $ S.ExternFunctionDecl name (S.Signature args result)

-- FIXME: add support for all S.TopLevelDecl values
topdecl :: Parser S.TopLevelDecl
topdecl
    = try topfundecl
  <|> try topexterndecl

toplevel :: Parser [S.TopLevelDecl]
toplevel = many $ do
  decl <- topdecl
  optional L.semi
  return decl

parseTopLevel s = parse (contents toplevel) "<stdin>" s
