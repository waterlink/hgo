module Main where

import System.Environment
import System.IO
import Control.Monad
import System.Exit

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

data LexicalElement = LineComment String
                    | GeneralComment String
                    | Token String

represent (LineComment text) = "line comment: " ++ text
represent (GeneralComment text) = "general comment: " ++ text
represent (Token name) = "token: " ++ name

-- Comments

lineComment :: P.Parser LexicalElement
lineComment = do P.try $ P.string "//"
                 text <- P.manyTill unicodeChar (P.try newline)
                 return $ LineComment text

generalComment :: P.Parser LexicalElement
generalComment = do P.try $ P.string "/*"
                    text <- P.manyTill unicodeChar (P.try $ P.string "*/")
                    return $ GeneralComment text

-- # Final syntax definition

final = lineComment
    <|> generalComment
