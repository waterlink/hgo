module Parser where

import Data.List
import Data.Ord

import Text.ParserCombinators.Parsec ((<|>))

import qualified Text.ParserCombinators.Parsec as P
import qualified Text.Parsec.String as PS

parse contents = case rawParse contents of
                      Left err -> "No match: " ++ show err
                      Right val -> "Parsed: " ++ (represent val)

rawParse contents = P.parse final "$STDIN" contents

-- # Source code representation

-- Characters

newline = P.newline

unicodeChar = P.anyChar

unicodeLetter = P.letter

unicodeDigit = P.digit

-- Letters and digits

letter = unicodeLetter <|> P.char '_'

decimalDigit = P.oneOf "0123456789"

octalDigit = P.oneOf "01234567"

hexDigit = P.oneOf "0123456789ABCDEFabcdef"

-- # Lexical elements

class Representable a where
      represent :: a -> String

data IntegerLiteral = Decimal String
                    | Octal String
                    | Hex String
                    deriving (Eq, Show)

instance Representable IntegerLiteral where
         represent (Decimal value) = "(decimal) " ++ value
         represent (Octal value) = "(octal) " ++ value
         represent (Hex value) = "(hex) " ++ value

data FloatLiteral = FloatValue String String String
                    deriving (Eq, Show)

instance Representable FloatLiteral where
         represent (FloatValue integer fractional exponent) = integer ++ "." ++ fractional ++ "e" ++ exponent

data LexicalElement = LineComment String
                    | GeneralComment String
                    | Identifier String
                    | Keyword String
                    | Operator String
                    | IntegerLiteral IntegerLiteral
                    | FloatLiteral FloatLiteral
                    | ImaginaryIntegerLiteral IntegerLiteral
                    | ImaginaryFloatLiteral FloatLiteral
                    deriving (Eq, Show)

instance Representable LexicalElement where
         represent (LineComment text) = "line comment: '" ++ text ++ "'"
         represent (GeneralComment text) = "general comment: '" ++ text ++ "'"
         represent (Identifier name) = "identifier: '" ++ name ++ "'"
         represent (Keyword name) = "keyword: '" ++ name ++ "'"
         represent (Operator name) = "operator: '" ++ name ++ "'"
         represent (IntegerLiteral value) = "integer literal: '" ++ (represent value) ++ "'"
         represent (FloatLiteral value) = "float literal: '" ++ (represent value) ++ "'"
         represent (ImaginaryIntegerLiteral value) = "imaginary literal: '" ++ (represent value) ++ "'"
         represent (ImaginaryFloatLiteral value) = "imaginary literal: '" ++ (represent value) ++ "'"

-- Comments

lineComment = do string "//"
                 text <- P.manyTill unicodeChar (P.try newline)
                 return $ LineComment text

generalComment = do string "/*"
                    text <- P.manyTill unicodeChar (string "*/")
                    return $ GeneralComment text

-- Tokens

identifier = do first <- letter
                rest <- P.many $ letter <|> unicodeDigit
                return $ Identifier (first:rest)

keywords = [
            "break",        "default",      "func",         "interface",    "select",
            "case",         "defer",        "go",           "map",          "struct",
            "chan",         "else",         "goto",         "package",      "switch",
            "const",        "fallthrough",  "if",           "range",        "type",
            "continue",     "for",          "import",       "return",       "var"]

parseKeyword = P.choice $ map word keywords

keyword = do name <- parseKeyword
             return $ Keyword name

operators = [
             "+",    "&",     "+=",    "&=",     "&&",    "==",    "!=",    "(",    ")",
             "-",    "|",     "-=",    "|=",     "||",    "<",     "<=",    "[",    "]",
             "*",    "^",     "*=",    "^=",     "<-",    ">",     ">=",    "{",    "}",
             "/",    "<<",    "/=",    "<<=",    "++",    "=",     ":=",    ",",    ";",
             "%",    ">>",    "%=",    ">>=",    "--",    "!",     "...",   ".",    ":",
                     "&^",             "&^="]
countOperatorsWithThatPrefix x = length $ filter (isPrefixOf x) operators
parseOrderedOperators = sortBy (comparing countOperatorsWithThatPrefix) operators

parseOperator = P.choice $ map string parseOrderedOperators

operator = do name <- parseOperator
              return $ Operator name

integerLiteral = do value <- parseIntegerLiteral
                    return $ IntegerLiteral value

parseIntegerLiteral = decimalLiteral <|> hexLiteral <|> octalLiteral <|> integerZero

integerZero = do value <- P.char '0'
                 return $ Decimal "0"

decimalLiteral = P.try $ do first <- P.oneOf "123456789"
                            rest <- P.many decimalDigit
                            return $ (Decimal $ first:rest)

octalLiteral = P.try $ do first <- P.char '0'
                          rest <- P.many1 octalDigit
                          return $ (Octal $ first:rest)

hexLiteral = P.try $ do first <- P.char '0'
                        middle <- P.oneOf "xX"
                        rest <- P.many hexDigit
                        return $ (Hex $ first:middle:rest)

parseFloatLiteral = a <|> b <|> c
                    where
                      a = P.try $ do integer <- decimals
                                     P.char '.'
                                     fractional <- P.option "0" decimals
                                     exponent <- P.option "+0" exponentPart
                                     return $ FloatValue integer fractional exponent
                      b = P.try $ do integer <- decimals
                                     exponent <- exponentPart
                                     return $ FloatValue integer "0" exponent
                      c = P.try $ do P.char '.'
                                     fractional <- decimals
                                     exponent <- P.option "+0" exponentPart
                                     return $ FloatValue "0" fractional exponent

floatLiteral = do value <- parseFloatLiteral
                  return $ FloatLiteral value

decimals = P.try $ do first <- decimalDigit
                      rest <- P.many decimalDigit
                      return $ first:rest

exponentPart = P.try $ do P.oneOf "eE"
                          sign <- P.option '+' (P.oneOf "+-")
                          rest <- decimals
                          return $ sign:rest

imaginaryLiteral = a <|> b
                   where
                     a = P.try $ do integer <- parseIntegerLiteral
                                    P.char 'i'
                                    return $ ImaginaryIntegerLiteral integer
                     b = P.try $ do float <- parseFloatLiteral
                                    P.char 'i'
                                    return $ ImaginaryFloatLiteral float

-- # Final syntax definition

final = lineComment
    <|> generalComment
    <|> keyword
    <|> identifier
    <|> imaginaryLiteral
    <|> floatLiteral
    <|> integerLiteral
    <|> operator

-- # Helpers

string x = P.try $ P.string x
word x = P.try $ do value <- P.string x
                    P.notFollowedBy (letter <|> unicodeDigit)
                    return value
