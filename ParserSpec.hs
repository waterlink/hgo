module ParserSpec where

import Test.Hspec
import qualified Parser as P
import qualified Text.ParserCombinators.Parsec as Parsec

instance Eq Parsec.ParseError where
         (==) x y = show x == show y

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
      P.rawParse "42" `shouldBe` Right (P.IntegerLiteral (P.Decimal "42"))
      P.rawParse "0600" `shouldBe` Right (P.IntegerLiteral (P.Octal "0600"))
      P.rawParse "0xBadFace3f" `shouldBe` Right (P.IntegerLiteral (P.Hex "0xBadFace3f"))
      P.rawParse "894864549864587456180" `shouldBe` Right (P.IntegerLiteral (P.Decimal "894864549864587456180"))

    it "parses floating-point literals" $ do
      P.rawParse "0." `shouldBe` Right (P.FloatLiteral "0" "0" "+0")
      P.rawParse "72.40" `shouldBe` Right (P.FloatLiteral "72" "40" "+0")
      P.rawParse "072.40" `shouldBe` Right (P.FloatLiteral "072" "40" "+0")
      P.rawParse "2.71828" `shouldBe` Right (P.FloatLiteral "2" "71828" "+0")
      P.rawParse "1.e+0" `shouldBe` Right (P.FloatLiteral "1" "0" "+0")
      P.rawParse "6.67428e-11" `shouldBe` Right (P.FloatLiteral "6" "67428" "-11")
      P.rawParse "1E6" `shouldBe` Right (P.FloatLiteral "1" "0" "+6")
      P.rawParse ".25" `shouldBe` Right (P.FloatLiteral "0" "25" "+0")
      P.rawParse ".12345E+5" `shouldBe` Right (P.FloatLiteral "0" "12345" "+5")

