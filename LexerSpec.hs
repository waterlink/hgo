module LexerSpec where

import Test.Hspec
import Data.Either
import Data.Complex
import Control.Exception (evaluate)

import qualified Lexer as L
import qualified Text.ParserCombinators.Parsec as PC
import qualified Text.ParserCombinators.Parsec.Error as PCError

instance Eq PC.ParseError where
         (==) x y = show x == show y

isLeft = null . rights . return

floatEqual x y = abs (x - y) < 1.0e-8
rightFloatEq y (Right x) = floatEqual x y
rightFloatEq _ _ = False

rightComplexEq (y1 :+ y2) (Right (x1 :+ x2)) = (floatEqual x1 y1) && (floatEqual x2 y2)
rightComplexEq _ _ = False

main = hspec $ do
  describe "rawParse" $ do
    it "parses identifiers" $ do
      L.parse L.identifier "a" `shouldBe` Right "a"
      L.parse L.identifier "_" `shouldBe` Right "_"
      L.parse L.identifier "_x9" `shouldBe` Right "_x9"
      L.parse L.identifier "ThisVariableIsExported" `shouldBe` Right "ThisVariableIsExported"
      L.parse L.identifier "αβ" `shouldBe` Right "αβ"
      L.parse L.identifier "foreach" `shouldBe` Right "foreach"
      L.parse L.identifier "for2" `shouldBe` Right "for2"
      L.parse L.identifier "breakage" `shouldBe` Right "breakage"

    it "parses keywords" $ do
      L.parse (L.keyword "for") "for" `shouldBe` Right ()
      L.parse (L.keyword "break") "break" `shouldBe` Right ()
      L.parse (L.keyword "continue") "continue" `shouldBe` Right ()
      L.parse (L.keyword "defer") "defer" `shouldBe` Right ()
      L.parse (L.keyword "for") "foreach" `shouldSatisfy` isLeft

    it "parses operators and delimiters" $ do
      L.parse (L.operator "+") "+" `shouldBe` Right ()
      L.parse (L.operator "+=") "+=" `shouldBe` Right ()
      L.parse (L.operator "&") "&" `shouldBe` Right ()
      L.parse (L.operator "&&") "&&" `shouldBe` Right ()
      L.parse (L.operator "&=") "&=" `shouldBe` Right ()
      L.parse (L.operator "&") "&&" `shouldSatisfy` isLeft

    it "parses integer literals" $ do
      L.parse L.integer "0" `shouldBe` Right 0
      L.parse L.integer "42" `shouldBe` Right 42
      L.parse L.integer "0600" `shouldBe` Right 0o600
      L.parse L.integer "0xBadFace3f" `shouldBe` Right 0xBadFace3f
      L.parse L.integer "894864549864587456180" `shouldBe` Right 894864549864587456180

    it "parses floating-point literals" $ do
      L.parse L.float "0." `shouldBe` Right 0.0
      L.parse L.float "72.40" `shouldBe` Right 72.4
      L.parse L.float "072.40" `shouldBe` Right 72.4
      L.parse L.float "2.71828" `shouldBe` Right 2.71828
      L.parse L.float "1.e+0" `shouldBe` Right 1.0
      L.parse L.float "6.67428e-11" `shouldBe` Right 6.67428e-11
      L.parse L.float "1E6" `shouldBe` Right 1.0e6
      L.parse L.float ".25" `shouldBe` Right 0.25
      L.parse L.float ".12345E+5" `shouldSatisfy` (rightFloatEq 0.12345e5)

    it "parses imaginary literals" $ do
      L.parse L.imaginary "0i" `shouldBe` Right (0 :+ 0)
      L.parse L.imaginary "011i" `shouldBe` Right (0 :+ 0o11)
      L.parse L.imaginary "0.i" `shouldBe` Right (0 :+ 0.0)
      L.parse L.imaginary "2.718128i" `shouldBe` Right (0 :+ 2.718128)
      L.parse L.imaginary "1.e+0i" `shouldBe` Right (0 :+ 1.0)
      L.parse L.imaginary "6.67428e-11i" `shouldBe` Right (0 :+ 6.67428e-11)
      L.parse L.imaginary "1E6i" `shouldBe` Right (0 :+ 1.0e6)
      L.parse L.imaginary ".25i" `shouldBe` Right (0 :+ 0.25)
      L.parse L.imaginary ".12345E+5i" `shouldSatisfy` rightComplexEq (0 :+ 0.12345e5)

    it "parses runes" $ do
      L.parse L.char "'a'" `shouldBe` Right 'a'
      L.parse L.char "'ä'" `shouldBe` Right 'ä'
      L.parse L.char "'本'" `shouldBe` Right '本'
      L.parse L.char "'\\t'" `shouldBe` Right '\t'
      L.parse L.char "'\\000'" `shouldBe` Right '\0'
      L.parse L.char "'\\007'" `shouldBe` Right '\7'
      L.parse L.char "'\\377'" `shouldBe` Right '\255'
      L.parse L.char "'\\x07'" `shouldBe` Right '\x7'
      L.parse L.char "'\\xff'" `shouldBe` Right '\xff'
      L.parse L.char "'\\u12e4'" `shouldBe` Right 'ዤ'
      L.parse L.char "'\\U0010FFFD'" `shouldBe` Right '\x10fffd'
      L.parse L.char "'\\\''" `shouldBe` Right '\''
      L.parse L.char "'aa'" `shouldSatisfy` isLeft
      L.parse L.char "'\\xa'" `shouldSatisfy` isLeft
      L.parse L.char "'\\0'" `shouldSatisfy` isLeft

    it "parses strings" $ do
      L.parse L.string "\"abc\"" `shouldBe` Right "abc"
      L.parse L.string "`abc`" `shouldBe` Right "abc"
      L.parse L.string "\"\\\\n\\n\\\\n\"" `shouldBe` Right "\\n\n\\n"
      L.parse L.string "`\\n\n\\n`" `shouldBe` Right "\\n\n\\n"
      L.parse L.string "\"\\n\"" `shouldBe` Right "\n"
      L.parse L.string "`\"`" `shouldBe` Right "\""
      L.parse L.string "\"\\\"\"" `shouldBe` Right "\""
      L.parse L.string "\"Hello, world!\\n\"" `shouldBe` Right "Hello, world!\n"
      L.parse L.string "\"日本語\"" `shouldBe` Right "日本語"
      L.parse L.string "\"\\u65e5本\\U00008a9e\"" `shouldBe` Right "\x65e5本\x00008a9e"
      L.parse L.string "\"\\xff\\u00FF\"" `shouldBe` Right "\xff\x00ff"
      L.parse L.string "\"\\uD800\"" `shouldBe` Right "\xd800"
