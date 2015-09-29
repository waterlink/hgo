module LexerNL where

import Data.List
import Data.Ord
import Data.Char (isSpace, digitToInt)
import Data.Complex

import Text.ParserCombinators.Parsec ((<|>))

import qualified Text.ParserCombinators.Parsec as P
import qualified Text.Parsec.String as PS

import qualified Text.Parsec.Token as Tok

import Text.Parsec.Language (emptyDef)

import TokenParserNL

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

identifier = Tok.identifier lexer
keyword = Tok.reserved lexer
operator = Tok.reservedOp lexer
integer = Tok.integer lexer
float = Tok.float lexer
char = Tok.charLiteral lexer
rune = char
string = Tok.stringLiteral lexer

imaginary = a <|> b
  where
    a = P.try $ do value <- integer
                   P.char 'i'
                   return $ 0 :+ (fromInteger value)
    b = P.try $ do value <- float
                   P.char 'i'
                   return $ 0 :+ value

parens = Tok.parens lexer
brackets = Tok.brackets lexer
braces = Tok.braces lexer
commaSep = Tok.commaSep lexer
semiSep = Tok.semiSep lexer

semi = operator ";"

anyKeyword :: PS.Parser String
anyKeyword = P.choice $ map keywordToken keywords

keywordToken name = keyword name >> return name

anyOperator :: PS.Parser String
anyOperator = P.choice $ map operatorToken operators

operatorToken name = operator name >> return name

anyLiteral :: PS.Parser String
anyLiteral
    = P.try floatToken
  <|> P.try imaginaryToken
  <|> P.try integerToken
  <|> P.try runeToken
  <|> P.try stringToken

floatToken = float >>= return . show
imaginaryToken = imaginary >>= return . show
integerToken = integer >>= return . show
runeToken = rune >>= return . (flip (:)) []
stringToken = string >>= return . show

nonNewlineWhitespace :: PS.Parser String
nonNewlineWhitespace = P.oneOf " \t" >>= return . (flip (:)) []

whitespace = Tok.whiteSpace lexer

nonNullWhitespace :: PS.Parser String
nonNullWhitespace = P.many1 $ P.oneOf " \t\n"

whitespaceToken :: PS.Parser String
whitespaceToken = whitespace >> return " "

newline :: PS.Parser String
newline = P.newline >> return "\n"

-- parse
parse parser contents = P.parse parser "$STDIN" contents
