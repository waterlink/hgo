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

typeNameWithoutEllipsis :: Parser S.TypeName
typeNameWithoutEllipsis = qualified <|> identifier
  where
    identifier = try $ do
      name <- L.identifier
      return $ S.Identifier name
    qualified = try $ do
      name <- qualifiedIdentifier
      return $ S.QualifiedIdentifier name

typeName :: Parser S.TypeNameWithEllipsis
typeName = withEllipsis <|> without
  where
    withEllipsis = try $ do
      L.operator "..."
      name <- typeNameWithoutEllipsis
      return $ S.TypeNameWithEllipsis name (Just ())
    without = try $ do
      name <- typeNameWithoutEllipsis
      return $ S.TypeNameWithEllipsis name Nothing

-- FIXME: add support for:
-- - type literals
paramdecl :: Parser S.FunctionParameterDecl
paramdecl = withName <|> without
  where
    withName = try $ do
      names <- L.commaSep L.identifier
      S.TypeNameWithEllipsis typeValue ellipsis <- typeName
      return $ S.ParameterDecl names (S.TypeName typeValue) ellipsis
    without = try $ do
      S.TypeNameWithEllipsis typeValue ellipsis <- typeName
      return $ S.ParameterDecl [] (S.TypeName typeValue) ellipsis

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace L.lexer
  r <- p
  eof
  return r

paramdecllist = try (L.parens $ L.commaSep paramdecl)

-- FIXME: add support for:
-- - type literals
resultType :: Parser (Maybe S.FunctionResult)
resultType = tuple <|> single <|> nothing
  where
    tuple = do
      values <- paramdecllist
      return $ Just (S.FunctionTupleResult values)
    single = do
      value <- typeNameWithoutEllipsis
      return $ Just (S.FunctionSingleResult $ S.TypeName value)
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
