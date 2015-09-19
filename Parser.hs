module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import qualified Lexer as L
import qualified Syntax as S

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

-- FIXME: add support for:
-- - body
topfundecl :: Parser S.TopLevelDecl
topfundecl = do
  L.keyword "func"
  name <- L.identifier
  args <- paramdecllist
  result <- resultType
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
