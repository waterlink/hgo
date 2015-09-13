module ParserSpec where

import Test.Hspec
import Data.Either

import qualified Parser as P
import qualified Text.ParserCombinators.Parsec as PC
import qualified Text.ParserCombinators.Parsec.Error as PCError

instance Eq PC.ParseError where
         (==) x y = show x == show y

isLeft = null . rights . return

main = hspec $ do
  describe "rawParse" $ do
    it "parses line comments" $
      P.rawParse "// hello world\n" `shouldBe` Right (P.LineComment " hello world")

    it "parses general comments" $
      P.rawParse "/* hello\n  world\n */" `shouldBe` Right (P.GeneralComment " hello\n  world\n ")

    it "parses identifiers" $ do
      P.rawParse "a" `shouldBe` Right (P.Identifier "a")
      P.rawParse "_" `shouldBe` Right (P.Identifier "_")
      P.rawParse "_x9" `shouldBe` Right (P.Identifier "_x9")
      P.rawParse "ThisVariableIsExported" `shouldBe` Right (P.Identifier "ThisVariableIsExported")
      P.rawParse "αβ" `shouldBe` Right (P.Identifier "αβ")
      P.rawParse "foreach" `shouldBe` Right (P.Identifier "foreach")
      P.rawParse "for2" `shouldBe` Right (P.Identifier "for2")
      P.rawParse "breakage" `shouldBe` Right (P.Identifier "breakage")

    it "parses keywords" $ do
      P.rawParse "for" `shouldBe` Right (P.Keyword "for")
      P.rawParse "break" `shouldBe` Right (P.Keyword "break")
      P.rawParse "continue" `shouldBe` Right (P.Keyword "continue")
      P.rawParse "defer" `shouldBe` Right (P.Keyword "defer")

    it "parses operators and delimiters" $ do
      P.rawParse "+" `shouldBe` Right (P.Operator "+")
      P.rawParse "+=" `shouldBe` Right (P.Operator "+=")
      P.rawParse "&" `shouldBe` Right (P.Operator "&")
      P.rawParse "&&" `shouldBe` Right (P.Operator "&&")
      P.rawParse "&=" `shouldBe` Right (P.Operator "&=")

    it "parses integer literals" $ do
      P.rawParse "0" `shouldBe` Right (P.IntegerLiteral (P.Decimal "0"))
      P.rawParse "42" `shouldBe` Right (P.IntegerLiteral (P.Decimal "42"))
      P.rawParse "0600" `shouldBe` Right (P.IntegerLiteral (P.Octal "0600"))
      P.rawParse "0xBadFace3f" `shouldBe` Right (P.IntegerLiteral (P.Hex "0xBadFace3f"))
      P.rawParse "894864549864587456180" `shouldBe` Right (P.IntegerLiteral (P.Decimal "894864549864587456180"))

    it "parses floating-point literals" $ do
      P.rawParse "0." `shouldBe` Right (P.FloatLiteral $ P.FloatValue "0" "0" "+0")
      P.rawParse "72.40" `shouldBe` Right (P.FloatLiteral $ P.FloatValue "72" "40" "+0")
      P.rawParse "072.40" `shouldBe` Right (P.FloatLiteral $ P.FloatValue "072" "40" "+0")
      P.rawParse "2.71828" `shouldBe` Right (P.FloatLiteral $ P.FloatValue "2" "71828" "+0")
      P.rawParse "1.e+0" `shouldBe` Right (P.FloatLiteral $ P.FloatValue "1" "0" "+0")
      P.rawParse "6.67428e-11" `shouldBe` Right (P.FloatLiteral $ P.FloatValue "6" "67428" "-11")
      P.rawParse "1E6" `shouldBe` Right (P.FloatLiteral $ P.FloatValue "1" "0" "+6")
      P.rawParse ".25" `shouldBe` Right (P.FloatLiteral $ P.FloatValue "0" "25" "+0")
      P.rawParse ".12345E+5" `shouldBe` Right (P.FloatLiteral $ P.FloatValue "0" "12345" "+5")

    it "parses imaginary literals" $ do
      P.rawParse "0i" `shouldBe` Right (P.ImaginaryIntegerLiteral $ P.Decimal "0")
      P.rawParse "011i" `shouldBe` Right (P.ImaginaryIntegerLiteral $ P.Octal "011")
      P.rawParse "0.i" `shouldBe` Right (P.ImaginaryFloatLiteral $ P.FloatValue "0" "0" "+0")
      P.rawParse "2.718128i" `shouldBe` Right (P.ImaginaryFloatLiteral $ P.FloatValue "2" "718128" "+0")
      P.rawParse "1.e+0i" `shouldBe` Right (P.ImaginaryFloatLiteral $ P.FloatValue "1" "0" "+0")
      P.rawParse "6.67428e-11i" `shouldBe` Right (P.ImaginaryFloatLiteral $ P.FloatValue "6" "67428" "-11")
      P.rawParse "1E6i" `shouldBe` Right (P.ImaginaryFloatLiteral $ P.FloatValue "1" "0" "+6")
      P.rawParse ".25i" `shouldBe` Right (P.ImaginaryFloatLiteral $ P.FloatValue "0" "25" "+0")
      P.rawParse ".12345E+5i" `shouldBe` Right (P.ImaginaryFloatLiteral $ P.FloatValue "0" "12345" "+5")

    it "parses runes" $ do
      P.rawParse "'a'" `shouldBe` Right (P.RuneLiteral $ P.CharRuneValue 'a')
      P.rawParse "'ä'" `shouldBe` Right (P.RuneLiteral $ P.CharRuneValue 'ä')
      P.rawParse "'本'" `shouldBe` Right (P.RuneLiteral $ P.CharRuneValue '本')
      P.rawParse "'\\t'" `shouldBe` Right (P.RuneLiteral $ P.EscapedCharRuneValue 't')
      P.rawParse "'\\000'" `shouldBe` Right (P.RuneLiteral $ P.OctalRuneValue "000")
      P.rawParse "'\\007'" `shouldBe` Right (P.RuneLiteral $ P.OctalRuneValue "007")
      P.rawParse "'\\377'" `shouldBe` Right (P.RuneLiteral $ P.OctalRuneValue "377")
      P.rawParse "'\\x07'" `shouldBe` Right (P.RuneLiteral $ P.HexRuneValue "07")
      P.rawParse "'\\xff'" `shouldBe` Right (P.RuneLiteral $ P.HexRuneValue "ff")
      P.rawParse "'\\u12e4'" `shouldBe` Right (P.RuneLiteral $ P.LittleURuneValue "12e4")
      P.rawParse "'\\U00101234'" `shouldBe` Right (P.RuneLiteral $ P.BigURuneValue "00101234")
      P.rawParse "'\\\''" `shouldBe` Right (P.RuneLiteral $ P.EscapedCharRuneValue '\'')
      P.rawParse "'aa'" `shouldSatisfy` isLeft
      P.rawParse "'\\xa'" `shouldSatisfy` isLeft
      P.rawParse "'\\0'" `shouldSatisfy` isLeft

      -- NOTE: these two according to Go RFC should be invalid, but they are
      -- invalid because of being invalid unicode, so at parse time these
      -- should succeed:
      P.rawParse "'\\uDFFF'" `shouldBe` Right (P.RuneLiteral $ P.LittleURuneValue "DFFF")
      P.rawParse "'\\U00110000'" `shouldBe` Right (P.RuneLiteral $ P.BigURuneValue "00110000")
