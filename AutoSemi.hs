module AutoSemi where

import Data.List (intersperse)

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified LexerNL as L

autoSemi = do
  tok <- preSemi
  skipMany L.nonNewlineWhitespace
  x <- L.newline
  skipMany L.nonNullWhitespace
  notFollowedBy (L.operator ")" <|> L.operator "}")
  return (tok ++ ";\n")

preSemi
    = try (L.keywordToken "break")
  <|> try (L.keywordToken "continue")
  <|> try (L.keywordToken "fallthrough")
  <|> try (L.keywordToken "return")
  <|> try (L.operatorToken "++")
  <|> try (L.operatorToken "--")
  <|> try (L.operatorToken ")")
  <|> try (L.operatorToken "]")
  <|> try (L.operatorToken "}")
  <|> try L.anyLiteral
  <|> try L.identifier

aToken
    = try autoSemi
  <|> try L.anyKeyword
  <|> try L.anyOperator
  <|> try L.anyLiteral
  <|> try L.identifier
  <|> try L.nonNullWhitespace

allTokens :: Parser String
allTokens = many aToken >>= return . concat . intersperse " "

contents :: Parser a -> Parser a
contents p = do
  r <- p
  eof
  return r

addAutoSemi s = parse (contents allTokens) "<stdin>" s
