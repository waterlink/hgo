module Lexer where

import Data.List
import Data.Ord
import Data.Char (isSpace)

import Text.ParserCombinators.Parsec ((<|>))

import qualified Text.ParserCombinators.Parsec as P
import qualified Text.Parsec.String as PS

parse contents = case rawParse contents of
                      Left err -> "No match: " ++ show err
                      Right val -> "Parsed: " ++ (represent val)

dbgParseAll contents = case parseAll contents of
                            Left err -> "No match: " ++ show err
                            Right val -> "Parsed: " ++ (represent val)

parseAll contents = P.parse tokens "$STDIN" contents

rawParse contents = P.parse final "$STDIN" contents

-- # Source code representation

-- Characters

newline = P.newline

unicodeChar = P.noneOf "\n"

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

data RuneValue = CharRuneValue Char
               | OctalRuneValue String
               | HexRuneValue String
               | LittleURuneValue String
               | BigURuneValue String
               | EscapedCharRuneValue Char
               deriving (Eq, Show)

instance Representable RuneValue where
         represent (CharRuneValue x) = x:""
         represent (OctalRuneValue x) = "(octal) " ++ x
         represent (HexRuneValue x) = "(hex) " ++ x
         represent (LittleURuneValue x) = "\\u" ++ x
         represent (BigURuneValue x) = "\\U" ++ x
         represent (EscapedCharRuneValue x) = "\\" ++ (x:"")

data LexicalElement = LineComment String
                    | GeneralComment String
                    | Identifier String
                    | Keyword String
                    | Operator String
                    | IntegerLiteral IntegerLiteral
                    | FloatLiteral FloatLiteral
                    | ImaginaryIntegerLiteral IntegerLiteral
                    | ImaginaryFloatLiteral FloatLiteral
                    | RuneLiteral RuneValue
                    | StringLiteral [RuneValue]
                    | Whitespace
                    | Newline
                    | Tokens [LexicalElement]
                    | None
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
         represent (RuneLiteral value) = "rune: '" ++ (represent value) ++ "'"
         represent (StringLiteral value) = "string: [" ++ (intercalate ", " $ map represent value) ++ "]"
         represent (Whitespace) = "whitespace"
         represent (Newline) = "newline"
         represent (Tokens tokens) = "tokens: [" ++ (intercalate ", " $ map represent tokens) ++ "]"
         represent (None) = "EOF reached"

-- Comments

lineComment = do string "//"
                 text <- P.manyTill unicodeChar (P.try newline)
                 return $ LineComment text

generalComment = do string "/*"
                    text <- P.manyTill P.anyChar (string "*/")
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

runeLiteral = P.try $ do P.char '\''
                         value <- byteValue <|> unicodeValue
                         P.char '\''
                         return $ RuneLiteral value

unicodeValue = littleUValue <|> bigUValue <|> escapedChar <|> unicodeCharRune
               where
                 unicodeCharRune = P.try $ do value <- unicodeChar
                                              return $ CharRuneValue value
                 littleUValue = P.try $ do P.string "\\u"
                                           h1 <- hexDigit
                                           h2 <- hexDigit
                                           h3 <- hexDigit
                                           h4 <- hexDigit
                                           return $ LittleURuneValue [h1, h2, h3, h4]
                 bigUValue = P.try $ do P.string "\\U"
                                        h1 <- hexDigit
                                        h2 <- hexDigit
                                        h3 <- hexDigit
                                        h4 <- hexDigit
                                        h5 <- hexDigit
                                        h6 <- hexDigit
                                        h7 <- hexDigit
                                        h8 <- hexDigit
                                        return $ BigURuneValue [h1, h2, h3, h4, h5, h6, h7, h8]
                 escapedChar = P.try $ do P.char '\\'
                                          value <- P.oneOf $ '\\':'\'':'"':"abfnrtv"
                                          return $ EscapedCharRuneValue value

byteValue = octalByteValue <|> hexByteValue
            where
              octalByteValue = P.try $ do P.char '\\'
                                          o1 <- octalDigit
                                          o2 <- octalDigit
                                          o3 <- octalDigit
                                          return $ OctalRuneValue [o1, o2, o3]
              hexByteValue = P.try $ do P.string "\\x"
                                        h1 <- hexDigit
                                        h2 <- hexDigit
                                        return $ HexRuneValue [h1, h2]

stringLiteral = rawString <|> interpretedString
                where
                  rawString = P.try $ do P.char '`'
                                         chars <- P.manyTill (P.noneOf "`") $ P.char '`'
                                         return $ StringLiteral (map CharRuneValue chars)
                  interpretedString = P.try $ do P.char '"'
                                                 runes <- P.manyTill (byteValue <|> unicodeValue) $ P.char '"'
                                                 return $ StringLiteral runes

whitespace = do P.skipMany $ P.satisfy (\c -> (isSpace c) && (c /= '\n'))
                return $ Whitespace

newlineElement = P.try $ do newline
                            return Newline

none = do P.eof
          return None

-- # Final syntax definition

final = whitespace >> (lineComment
                   <|> generalComment
                   <|> newlineElement
                   <|> keyword
                   <|> identifier
                   <|> imaginaryLiteral
                   <|> floatLiteral
                   <|> integerLiteral
                   <|> operator
                   <|> stringLiteral
                   <|> runeLiteral
                   <|> none)

tokens = do head <- final
            let fn = case head of
                          None -> empty
                          Identifier _ -> autoSemicolon
                          IntegerLiteral _ -> autoSemicolon
                          FloatLiteral _ -> autoSemicolon
                          ImaginaryIntegerLiteral _ -> autoSemicolon
                          ImaginaryFloatLiteral _ -> autoSemicolon
                          RuneLiteral _ -> autoSemicolon
                          StringLiteral _ -> autoSemicolon
                          Keyword name | name `elem` ["break", "continue", "fallthrough", "return"] -> autoSemicolon
                          Operator name | name `elem` ["++", "--", ")", "]", "}"] -> autoSemicolon
                          _ -> appendTokens
            fn head
            where
              empty head = return $ Tokens []
              autoSemicolon head = do rest <- tokens
                                      case rest of
                                          Tokens (Newline:tail) -> return $ Tokens (head:(Operator ";"):Newline:tail)
                                          Tokens tail -> return $ Tokens (head:tail)
              appendTokens head = do rest <- tokens
                                     case rest of
                                          Tokens tail -> return $ Tokens (head:tail)


-- # Helpers

string x = P.try $ P.string x
word x = P.try $ do value <- P.string x
                    P.notFollowedBy (letter <|> unicodeDigit)
                    return value
