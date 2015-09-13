module LexerSpec where

import Test.Hspec
import Data.Either

import qualified Lexer as L
import qualified Text.ParserCombinators.Parsec as PC
import qualified Text.ParserCombinators.Parsec.Error as PCError

instance Eq PC.ParseError where
         (==) x y = show x == show y

isLeft = null . rights . return

main = hspec $ do
  describe "rawParse" $ do
    it "parses line comments" $
      L.rawParse "// hello world\n" `shouldBe` Right (L.LineComment " hello world")

    it "parses general comments" $
      L.rawParse "/* hello\n  world\n */" `shouldBe` Right (L.GeneralComment " hello\n  world\n ")

    it "parses identifiers" $ do
      L.rawParse "a" `shouldBe` Right (L.Identifier "a")
      L.rawParse "_" `shouldBe` Right (L.Identifier "_")
      L.rawParse "_x9" `shouldBe` Right (L.Identifier "_x9")
      L.rawParse "ThisVariableIsExported" `shouldBe` Right (L.Identifier "ThisVariableIsExported")
      L.rawParse "αβ" `shouldBe` Right (L.Identifier "αβ")
      L.rawParse "foreach" `shouldBe` Right (L.Identifier "foreach")
      L.rawParse "for2" `shouldBe` Right (L.Identifier "for2")
      L.rawParse "breakage" `shouldBe` Right (L.Identifier "breakage")

    it "parses keywords" $ do
      L.rawParse "for" `shouldBe` Right (L.Keyword "for")
      L.rawParse "break" `shouldBe` Right (L.Keyword "break")
      L.rawParse "continue" `shouldBe` Right (L.Keyword "continue")
      L.rawParse "defer" `shouldBe` Right (L.Keyword "defer")

    it "parses operators and delimiters" $ do
      L.rawParse "+" `shouldBe` Right (L.Operator "+")
      L.rawParse "+=" `shouldBe` Right (L.Operator "+=")
      L.rawParse "&" `shouldBe` Right (L.Operator "&")
      L.rawParse "&&" `shouldBe` Right (L.Operator "&&")
      L.rawParse "&=" `shouldBe` Right (L.Operator "&=")

    it "parses integer literals" $ do
      L.rawParse "0" `shouldBe` Right (L.IntegerLiteral (L.Decimal "0"))
      L.rawParse "42" `shouldBe` Right (L.IntegerLiteral (L.Decimal "42"))
      L.rawParse "0600" `shouldBe` Right (L.IntegerLiteral (L.Octal "0600"))
      L.rawParse "0xBadFace3f" `shouldBe` Right (L.IntegerLiteral (L.Hex "0xBadFace3f"))
      L.rawParse "894864549864587456180" `shouldBe` Right (L.IntegerLiteral (L.Decimal "894864549864587456180"))

    it "parses floating-point literals" $ do
      L.rawParse "0." `shouldBe` Right (L.FloatLiteral $ L.FloatValue "0" "0" "+0")
      L.rawParse "72.40" `shouldBe` Right (L.FloatLiteral $ L.FloatValue "72" "40" "+0")
      L.rawParse "072.40" `shouldBe` Right (L.FloatLiteral $ L.FloatValue "072" "40" "+0")
      L.rawParse "2.71828" `shouldBe` Right (L.FloatLiteral $ L.FloatValue "2" "71828" "+0")
      L.rawParse "1.e+0" `shouldBe` Right (L.FloatLiteral $ L.FloatValue "1" "0" "+0")
      L.rawParse "6.67428e-11" `shouldBe` Right (L.FloatLiteral $ L.FloatValue "6" "67428" "-11")
      L.rawParse "1E6" `shouldBe` Right (L.FloatLiteral $ L.FloatValue "1" "0" "+6")
      L.rawParse ".25" `shouldBe` Right (L.FloatLiteral $ L.FloatValue "0" "25" "+0")
      L.rawParse ".12345E+5" `shouldBe` Right (L.FloatLiteral $ L.FloatValue "0" "12345" "+5")

    it "parses imaginary literals" $ do
      L.rawParse "0i" `shouldBe` Right (L.ImaginaryIntegerLiteral $ L.Decimal "0")
      L.rawParse "011i" `shouldBe` Right (L.ImaginaryIntegerLiteral $ L.Octal "011")
      L.rawParse "0.i" `shouldBe` Right (L.ImaginaryFloatLiteral $ L.FloatValue "0" "0" "+0")
      L.rawParse "2.718128i" `shouldBe` Right (L.ImaginaryFloatLiteral $ L.FloatValue "2" "718128" "+0")
      L.rawParse "1.e+0i" `shouldBe` Right (L.ImaginaryFloatLiteral $ L.FloatValue "1" "0" "+0")
      L.rawParse "6.67428e-11i" `shouldBe` Right (L.ImaginaryFloatLiteral $ L.FloatValue "6" "67428" "-11")
      L.rawParse "1E6i" `shouldBe` Right (L.ImaginaryFloatLiteral $ L.FloatValue "1" "0" "+6")
      L.rawParse ".25i" `shouldBe` Right (L.ImaginaryFloatLiteral $ L.FloatValue "0" "25" "+0")
      L.rawParse ".12345E+5i" `shouldBe` Right (L.ImaginaryFloatLiteral $ L.FloatValue "0" "12345" "+5")

    it "parses runes" $ do
      L.rawParse "'a'" `shouldBe` Right (L.RuneLiteral $ L.CharRuneValue 'a')
      L.rawParse "'ä'" `shouldBe` Right (L.RuneLiteral $ L.CharRuneValue 'ä')
      L.rawParse "'本'" `shouldBe` Right (L.RuneLiteral $ L.CharRuneValue '本')
      L.rawParse "'\\t'" `shouldBe` Right (L.RuneLiteral $ L.EscapedCharRuneValue 't')
      L.rawParse "'\\000'" `shouldBe` Right (L.RuneLiteral $ L.OctalRuneValue "000")
      L.rawParse "'\\007'" `shouldBe` Right (L.RuneLiteral $ L.OctalRuneValue "007")
      L.rawParse "'\\377'" `shouldBe` Right (L.RuneLiteral $ L.OctalRuneValue "377")
      L.rawParse "'\\x07'" `shouldBe` Right (L.RuneLiteral $ L.HexRuneValue "07")
      L.rawParse "'\\xff'" `shouldBe` Right (L.RuneLiteral $ L.HexRuneValue "ff")
      L.rawParse "'\\u12e4'" `shouldBe` Right (L.RuneLiteral $ L.LittleURuneValue "12e4")
      L.rawParse "'\\U00101234'" `shouldBe` Right (L.RuneLiteral $ L.BigURuneValue "00101234")
      L.rawParse "'\\\''" `shouldBe` Right (L.RuneLiteral $ L.EscapedCharRuneValue '\'')
      L.rawParse "'aa'" `shouldSatisfy` isLeft
      L.rawParse "'\\xa'" `shouldSatisfy` isLeft
      L.rawParse "'\\0'" `shouldSatisfy` isLeft

      -- NOTE: these two according to Go RFC should be invalid, but they are
      -- invalid because of being invalid unicode, so at parse time these
      -- should succeed:
      L.rawParse "'\\uDFFF'" `shouldBe` Right (L.RuneLiteral $ L.LittleURuneValue "DFFF")
      L.rawParse "'\\U00110000'" `shouldBe` Right (L.RuneLiteral $ L.BigURuneValue "00110000")

    it "parses strings" $ do
      L.rawParse "\"abc\"" `shouldBe` Right (L.StringLiteral $ map L.CharRuneValue "abc")
      L.rawParse "`abc`" `shouldBe` Right (L.StringLiteral $ map L.CharRuneValue "abc")
      L.rawParse "\"\\\\n\\n\\\\n\"" `shouldBe` Right (L.StringLiteral [L.EscapedCharRuneValue '\\', L.CharRuneValue 'n', L.EscapedCharRuneValue 'n', L.EscapedCharRuneValue '\\', L.CharRuneValue 'n'])
      L.rawParse "`\\n\n\\n`" `shouldBe` Right (L.StringLiteral $ map L.CharRuneValue "\\n\n\\n")
      L.rawParse "\"\\n\"" `shouldBe` Right (L.StringLiteral [L.EscapedCharRuneValue 'n'])
      L.rawParse "`\"`" `shouldBe` Right (L.StringLiteral [L.CharRuneValue '"'])
      L.rawParse "\"\\\"\"" `shouldBe` Right (L.StringLiteral [L.EscapedCharRuneValue '"'])
      L.rawParse "\"Hello, world!\\n\"" `shouldBe` Right (L.StringLiteral $ (map L.CharRuneValue "Hello, world!") ++ [L.EscapedCharRuneValue 'n'])
      L.rawParse "\"日本語\"" `shouldBe` Right (L.StringLiteral $ map L.CharRuneValue "日本語")
      L.rawParse "\"\\u65e5本\\U00008a9e\"" `shouldBe` Right (L.StringLiteral [L.LittleURuneValue "65e5", L.CharRuneValue '本', L.BigURuneValue "00008a9e"])
      L.rawParse "\"\\xff\\u00FF\"" `shouldBe` Right (L.StringLiteral [L.HexRuneValue "ff", L.LittleURuneValue "00FF"])
      L.rawParse "\"\\uD800\"" `shouldBe` Right (L.StringLiteral [L.LittleURuneValue "D800"])
      L.rawParse "\"\\U00110000\"" `shouldBe` Right (L.StringLiteral [L.BigURuneValue "00110000"])

    it "handles whitespace correctly" $ do
      L.rawParse "   hello   " `shouldBe` Right (L.Identifier "hello")

  describe "parseAll" $ do
    it "handles simple math expression" $ do
      L.parseAll "2 + 2 == 4" `shouldBe` Right (L.Tokens [
        L.IntegerLiteral $ L.Decimal "2",
        L.Operator "+",
        L.IntegerLiteral $ L.Decimal "2",
        L.Operator "==",
        L.IntegerLiteral $ L.Decimal "4"])

    it "handles package definition" $ do
      L.parseAll "package main" `shouldBe` Right (L.Tokens [
        L.Keyword "package",
        L.Identifier "main"])

    it "handles an import" $ do
      L.parseAll "import \"fmt\"" `shouldBe` Right (L.Tokens [
        L.Keyword "import",
        L.StringLiteral $ map L.CharRuneValue "fmt"])

      L.parseAll "\n  import (\n\"fmt\"\n\"io\"\n)\n" `shouldBe` Right (L.Tokens [
        L.Newline,
        L.Keyword "import",
        L.Operator "(",
        L.Newline,
        L.StringLiteral $ map L.CharRuneValue "fmt",
        L.Operator ";",
        L.Newline,
        L.StringLiteral $ map L.CharRuneValue "io",
        L.Operator ";",
        L.Newline,
        L.Operator ")",
        L.Operator ";",
        L.Newline])

    it "handles function definition" $ do
      L.parseAll "func main() {\n  fmt.Println(\"Hello, World!\")\n}\n" `shouldBe` Right (L.Tokens [
        L.Keyword "func",
        L.Identifier "main",
        L.Operator "(",
        L.Operator ")",
        L.Operator "{",
        L.Newline,
        L.Identifier "fmt",
        L.Operator ".",
        L.Identifier "Println",
        L.Operator "(",
        L.StringLiteral $ map L.CharRuneValue "Hello, World!",
        L.Operator ")",
        L.Operator ";",
        L.Newline,
        L.Operator "}",
        L.Operator ";",
        L.Newline])
