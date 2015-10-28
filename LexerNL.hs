module LexerNL where

import qualified GenericLexer as G

import qualified Text.Parsec.String as PS

l = G.lexer True

anyKeyword = G.anyKeyword l
anyLiteral = G.anyLiteral l
anyOperator = G.anyOperator l
identifier = G.identifier l
keywordToken = G.keywordToken l
operator = G.operator l
operatorToken = G.operatorToken l

newline = G.newline
nonNewlineWhitespace = G.nonNewlineWhitespace
nonNullWhitespace = G.nonNullWhitespace

-- parse
parse parser = G.parse parser
