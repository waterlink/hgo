module Lexer where

import qualified GenericLexer as G

import qualified Text.Parsec.String as PS

l = G.lexer False

braces = G.braces l
brackets = G.brackets l
commaSep = G.commaSep l
float = G.float l
identifier = G.identifier l
imaginary = G.imaginary l
integer = G.integer l
keyword = G.keyword l
operator = G.operator l
parens = G.parens l
rune = G.rune l
semi = G.semi l
string = G.string l

-- parse
parse parser = G.parse parser
