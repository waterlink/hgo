module GenericLexer where

import Data.List
import Data.Ord
import Data.Char (isSpace, digitToInt)
import Data.Complex

import Text.ParserCombinators.Parsec ((<|>))

import qualified Text.ParserCombinators.Parsec as P
import qualified Text.Parsec.String as PS

import qualified Text.Parsec.Token as Tok

import Text.Parsec.Language (emptyDef)

import TokenParser

lexer = makeTokenParser style
  where
    style = emptyDef {
      Tok.commentLine = "//"
    , Tok.commentStart = "/*"
    , Tok.commentEnd = "*/"
    , Tok.nestedComments = False
    , Tok.identStart = letter
    , Tok.identLetter = letter <|> unicodeDigit
    , Tok.reservedOpNames = operators
    , Tok.reservedNames = keywords
    , Tok.caseSensitive = True
    }

unicodeLetter = P.letter

unicodeDigit = P.digit

letter = unicodeLetter <|> P.char '_'

keywords = [
            "break",        "default",      "func",         "interface",    "select",
            "case",         "defer",        "go",           "map",          "struct",
            "chan",         "else",         "goto",         "package",      "switch",
            "const",        "fallthrough",  "if",           "range",        "type",
            "continue",     "for",          "import",       "return",       "var",
            "extern"]

operators = [
             "+",    "&",     "+=",    "&=",     "&&",    "==",    "!=",    "(",    ")",
             "-",    "|",     "-=",    "|=",     "||",    "<",     "<=",    "[",    "]",
             "*",    "^",     "*=",    "^=",     "<-",    ">",     ">=",    "{",    "}",
             "/",    "<<",    "/=",    "<<=",    "++",    "=",     ":=",    ",",    ";",
             "%",    ">>",    "%=",    ">>=",    "--",    "!",     "...",   ".",    ":",
                     "&^",             "&^="]

identifier = Tok.identifier
keyword = Tok.reserved
operator = Tok.reservedOp
integer = Tok.integer
float = Tok.float
char = Tok.charLiteral
rune = char
string = Tok.stringLiteral

imaginary l = a <|> b
  where
    a = P.try $ do value <- integer l
                   P.char 'i'
                   return $ 0 :+ (fromInteger value)
    b = P.try $ do value <- float l
                   P.char 'i'
                   return $ 0 :+ value

parens = Tok.parens
brackets = Tok.brackets
braces = Tok.braces
commaSep = Tok.commaSep
semiSep = Tok.semiSep

semi l = operator l ";"

--anyKeyword :: PS.Parser String
anyKeyword l = P.choice $ map (keywordToken l) keywords

keywordToken l name = keyword l name >> return name

--anyOperator :: PS.Parser String
anyOperator l = P.choice $ map (operatorToken l) operators

operatorToken l name = operator l name >> return name

--anyLiteral :: PS.Parser String
anyLiteral l
    = P.try (floatToken l)
  <|> P.try (imaginaryToken l)
  <|> P.try (integerToken l)
  <|> P.try (runeToken l)
  <|> P.try (stringToken l)

floatToken l = float l >>= return . show
imaginaryToken l = imaginary l >>= return . show
integerToken l = integer l >>= return . show
runeToken l = rune l >>= return . (flip (:)) []
stringToken l = string l >>= return . show

nonNewlineWhitespace :: PS.Parser String
nonNewlineWhitespace = P.oneOf " \t" >>= return . (flip (:)) []

whitespace = Tok.whiteSpace

nonNullWhitespace :: PS.Parser String
nonNullWhitespace = P.many1 $ P.oneOf " \t\n"

--whitespaceToken :: PS.Parser String
whitespaceToken l = whitespace l >> return " "

newline :: PS.Parser String
newline = P.newline >> return "\n"

-- parse
parse parser contents = P.parse parser "$STDIN" contents
