module Main where

import System.Environment
import System.IO
import Control.Monad
import System.Exit
import Data.List
import Data.Ord

import Text.ParserCombinators.Parsec ((<|>))

import qualified Text.ParserCombinators.Parsec as P
import qualified Text.Parsec.String as PS

main :: IO()
main = do contents <- getContents
          let output = case P.parse final "$STDIN" contents of
                            Left err -> "No match: " ++ show err
                            Right val -> "Parsed: " ++ (represent val)
          putStrLn output

-- # Source code representation

-- Characters

newline :: P.Parser Char
newline = P.newline

unicodeChar :: P.Parser Char
unicodeChar = P.anyChar

unicodeLetter :: P.Parser Char
unicodeLetter = P.letter

unicodeDigit :: P.Parser Char
unicodeDigit = P.digit

-- Letters and digits

letter :: P.Parser Char
letter = unicodeLetter <|> P.char '_'

decimalDigit :: P.Parser Char
decimalDigit = P.oneOf "0123456789"

octalDigit :: P.Parser Char
octalDigit = P.oneOf "01234567"

hexDigit :: P.Parser Char
hexDigit = P.oneOf "0123456789ABCDEFabcdef"

-- # Lexical elements

class Representable a where
      represent :: a -> String

data IntegerLiteral = Decimal String
                    | Octal String
                    | Hex String

instance Representable IntegerLiteral where
         represent (Decimal value) = "(decimal) " ++ value
         represent (Octal value) = "(octal) " ++ value
         represent (Hex value) = "(hex) " ++ value

data LexicalElement = LineComment String
                    | GeneralComment String
                    | Identifier String
                    | Keyword String
                    | Operator String
                    | IntegerLiteral IntegerLiteral

instance Representable LexicalElement where
         represent (LineComment text) = "line comment: '" ++ text ++ "'"
         represent (GeneralComment text) = "general comment: '" ++ text ++ "'"
         represent (Identifier name) = "identifier: '" ++ name ++ "'"
         represent (Keyword name) = "keyword: '" ++ name ++ "'"
         represent (Operator name) = "operator: '" ++ name ++ "'"
         represent (IntegerLiteral value) = "integer literal: '" ++ (represent value) ++ "'"

-- Comments

lineComment :: P.Parser LexicalElement
lineComment = do string "//"
                 text <- P.manyTill unicodeChar (P.try newline)
                 return $ LineComment text

generalComment :: P.Parser LexicalElement
generalComment = do string "/*"
                    text <- P.manyTill unicodeChar (string "*/")
                    return $ GeneralComment text

-- Tokens

identifier :: P.Parser LexicalElement
identifier = do first <- letter
                rest <- P.many $ letter <|> unicodeDigit
                return $ Identifier (first:rest)

keywords = [
            "break",        "default",      "func",         "interface",    "select",
            "case",         "defer",        "go",           "map",          "struct",
            "chan",         "else",         "goto",         "package",      "switch",
            "const",        "fallthrough",  "if",           "range",        "type",
            "continue",     "for",          "import",       "return",       "var"]

parseKeyword :: P.Parser String
parseKeyword = P.choice $ map word keywords

keyword :: P.Parser LexicalElement
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

parseOperator :: P.Parser String
parseOperator = P.choice $ map string parseOrderedOperators

operator :: P.Parser LexicalElement
operator = do name <- parseOperator
              return $ Operator name

integerLiteral :: P.Parser LexicalElement
integerLiteral = decimalLiteral <|> hexLiteral <|> octalLiteral

decimalLiteral :: P.Parser LexicalElement
decimalLiteral = P.try $ do first <- P.oneOf "123456789"
                            rest <- P.many decimalDigit
                            return $ IntegerLiteral (Decimal $ first:rest)

octalLiteral :: P.Parser LexicalElement
octalLiteral = P.try $ do first <- P.char '0'
                          rest <- P.many octalDigit
                          return $ IntegerLiteral (Octal $ first:rest)

hexLiteral :: P.Parser LexicalElement
hexLiteral = P.try $ do first <- P.char '0'
                        middle <- P.oneOf "xX"
                        rest <- P.many hexDigit
                        return $ IntegerLiteral (Hex $ first:middle:rest)

-- # Final syntax definition

final = lineComment
    <|> generalComment
    <|> keyword
    <|> identifier
    <|> operator
    <|> integerLiteral

-- # Helpers

string x = P.try $ P.string x
word x = P.try $ do value <- P.string x
                    P.notFollowedBy (letter <|> unicodeDigit)
                    return value
