module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import qualified Lexer as L
import qualified Syntax as S

newUnaryOp :: S.UnaryOp -> S.Expression -> S.Expression
newUnaryOp f e = S.Unary $ S.UnaryOp f e

binary s f assoc = Ex.Infix (L.operator s >> return (S.Binary f)) assoc
unary s f = Ex.Prefix (L.operator s >> return (newUnaryOp f))

opTable = [[unary "+" S.UnPlus,
            unary "-" S.UnMinus,
            unary "!" S.Not,
            unary "^" S.UnCaret,
            unary "*" S.Deref,
            unary "&" S.Pointerof,
            unary "<-" S.ChanRead],

           [binary "*" S.Mult Ex.AssocLeft,
            binary "/" S.Div Ex.AssocLeft,
            binary "%" S.Mod Ex.AssocLeft,
            binary "<<" S.LShift Ex.AssocLeft,
            binary ">>" S.RShift Ex.AssocLeft,
            binary "&" S.BitAnd Ex.AssocLeft,
            binary "&^" S.AndXor Ex.AssocLeft],

           [binary "+" S.Plus Ex.AssocLeft,
            binary "-" S.Minus Ex.AssocLeft,
            binary "|" S.BitOr Ex.AssocLeft,
            binary "^" S.BitXor Ex.AssocLeft],

           [binary "==" S.Equals Ex.AssocLeft,
            binary "!=" S.NotEquals Ex.AssocLeft,
            binary "<" S.Less Ex.AssocLeft,
            binary "<=" S.LessEq Ex.AssocLeft,
            binary ">" S.Greater Ex.AssocLeft,
            binary ">=" S.GreaterEq Ex.AssocLeft],

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
  Tok.whiteSpace L.lexer
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
    mult = try $ do
      L.operator "*"
      return S.Mult
    div = try $ do
      L.operator "/"
      return S.Div
    mod = try $ do
      L.operator "%"
      return S.Mod
    lshift = try $ do
      L.operator "<<"
      return S.LShift
    rshift = try $ do
      L.operator ">>"
      return S.RShift
    and = try $ do
      L.operator "&"
      return S.And
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
    = exprstmt
  <|> send
  <|> incstmt
  <|> decstmt
  <|> assignstmt
  <|> shortvardecl
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
    = simple
  <|> gostmt
  <|> returnstmt
  <|> breakstmtWithLabel
  <|> breakstmt
  <|> continuestmtWithLabel
  <|> continuestmt
  <|> fallthroughstmt
  <|> blockstmt
  <|> deferstmt
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

maybeExpr :: Parser (Maybe S.Expression)
maybeExpr = option Nothing $ do
  value <- expression
  return $ Just value

exprFactor :: Parser S.Expression
exprFactor
    = operandExpr
  <|> conversion
  <|> selector
  <|> index
  <|> slice
  <|> sliceWithStep
  <|> typeAssertion
  <|> arguments

  where
    operandExpr = try $ do
      value <- operand
      return $ S.Operand value

    conversion = try $ do
      typeValue <- fullTypeValue
      L.operator "("
      value <- expression
      optional $ L.operator ","
      L.operator ")"
      return $ S.Conversion typeValue value

    selector = try $ do
      value <- expression
      L.operator "."
      name <- L.identifier
      return $ S.Selector value name

    index = try $ do
      value <- expression
      indexValue <- L.brackets expression
      return $ S.Index value expression

    slice = try $ do
      value <- expression
      L.operator "["
      start <- maybeExpr
      L.operator ":"
      end <- maybeExpr
      L.operator "]"
      return $ S.Slice value start end Nothing

    sliceWithStep = try $ do
      value <- expression
      L.operator "["
      start <- maybeExpr
      L.operator ":"
      end <- expression
      L.operator ":"
      step <- expression
      return $ S.Slice value start (Just end) (Just step)

    typeAssertion = try $ do
      value <- expression
      L.operator "."
      typeValue <- L.parens fullTypeValue
      return $ S.TypeAssertion value typeValue

    -- FIXME:
    -- - figure out what Type can do here (see go's RFC)
    -- - add support for Ellipsis
    arguments = try $ do
      value <- expression
      args <- L.parens $ L.commaSep expression
      return $ S.Arguments value args Nothing Nothing

operand :: Parser S.Operand
operand = literal <|> name <|> qualifiedName <|> methodExpr <|> exprInParens
  where
    literal = try $ do
      value <- literalExpr
      return $ S.Literal value

    name = try $ do
      value <- L.identifier
      return $ S.OperandName (S.OperandIdentifier value)

    qualifiedName = try $ do
      value <- qualifiedIdentifier
      return $ S.OperandName (S.OperandQualified value)

    -- FIXME: add support for "(" ReceiverType ")"
    -- FIXME: currently allows all type literals, but shouldn't ?
    methodExpr = try $ do
      typeValue <- fullTypeValue
      L.operator "."
      name <- L.identifier
      return $ S.MethodExpr typeValue name

    exprInParens = try $ L.parens expression

-- FIXME: add support for function literal
literalExpr :: Parser S.Literal
literalExpr = basic <|> composite
  where
    integer = try $ do
      value <- L.integer
      return $ S.IntLiteral value

    float = try $ do
      value <- L.float
      return $ S.FloatLiteral value

    imaginary = try $ do
      value <- L.imaginary
      return $ S.ImaginaryLiteral value

    rune = try $ do
      value <- L.rune
      return $ S.RuneLiteral value

    string = try $ do
      value <- L.string
      return $ S.StringLiteral value

    basic = integer
        <|> float
        <|> imaginary
        <|> rune
        <|> string

    composite = try $ do
      typeValue <- typeLiteral
      value <- literalValue
      return $ S.CompositeLiteral typeValue value

literalValue :: Parser S.LiteralValue
literalValue = try $ do
  elements <- L.braces $ L.commaSep element
  return $ S.LiteralValue elements
  where
    element = try $ do
      key <- option Nothing withKey
      value <- literalValue <|> expression
      return $ S.LiteralElement key value

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
block = L.braces $ L.semiSep statement

-- FIXME: add support for:
-- - body
topfundecl :: Parser S.TopLevelDecl
topfundecl = do
  L.keyword "func"
  name <- L.identifier
  args <- paramdecllist
  result <- resultType
  body <- block
  return $ S.FunctionDecl name (S.Signature args result) Nothing

topdecl :: Parser S.TopLevelDecl
topdecl = try topfundecl

-- FIXME: Add support for all S.TopLevelDecl values
toplevel :: Parser [S.TopLevelDecl]
toplevel = many $ do
  decl <- topdecl
  L.operator ";"
  return decl

parseTopLevel s = parse (contents toplevel) "<stdin>" s
