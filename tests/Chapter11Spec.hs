module Chapter11Spec where

import Chapter11
import AParser
import Data.Char
import Test.Hspec

spec :: Spec
spec = do
  describe "chapter 11" $ do
    it "zeroOrMore" $ do
      runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC", "dEfgH")
      runParser (zeroOrMore (satisfy isUpper)) "abcdefgh" `shouldBe` Just ("", "abcdefgh")

    it "oneOrMore" $ do
      runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC", "dEfgH")
      runParser (oneOrMore (satisfy isUpper)) "abcdefgh" `shouldBe` Nothing

    it "spaces" $ do
      runParser spaces "   asdf" `shouldBe` Just ("   ", "asdf")
      runParser spaces "asdf" `shouldBe` Just ("", "asdf")

    it "ident" $ do
      runParser ident "   asdf" `shouldBe` Nothing
      runParser ident "3asdf" `shouldBe` Nothing
      runParser ident "asdf" `shouldBe` Just ("asdf", "")
      runParser ident "asdf3 qwer" `shouldBe` Just ("asdf3", " qwer")
      runParser ident "asdf3jkl5 qwer" `shouldBe` Just ("asdf3jkl5", " qwer")

    it "parseIdent" $ do
      runParser parseIdent "   asdf" `shouldBe` Nothing
      runParser parseIdent "3asdf" `shouldBe` Nothing
      runParser parseIdent "asdf" `shouldBe` Just (I "asdf", "")
      runParser parseIdent "asdf3 qwer" `shouldBe` Just (I "asdf3", " qwer")

    it "parseInt" $ do
      runParser parseInt "   asdf" `shouldBe` Nothing
      runParser parseInt "3asdf" `shouldBe` Nothing
      runParser parseInt "3" `shouldBe` Just (N 3, "")
      runParser parseInt "3 asdf" `shouldBe` Just (N 3, "asdf")

    it "parseAtom" $ do
      runParser parseAtom "   asdf" `shouldBe` Nothing
      runParser parseAtom "3asdf" `shouldBe` Nothing
      runParser parseAtom "3 asdf" `shouldBe` Just (N 3, "asdf")

    -- it "sexpr" $ do
    --   runParser parseSExpr "5x" `shouldBe` Nothing
    --   runParser parseSExpr "5" `shouldBe` Just (A (N 5), "")
    --   runParser parseSExpr "foo3" `shouldBe` Just (A (I "foo3"), "")
    --   runParser parseSExpr "  5  " `shouldBe` Just (A (N 5), "")
    --   runParser parseSExpr "  foo3  " `shouldBe` Just (A (I "foo3"), "")
    --   -- runParser parseSExpr "(bar (foo) 3 5 874)" `shouldBe` Just ([I "bar", [I "foo"], N 3, N 5, N 874], "")
    --   -- runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)" `shouldBe` Just ([[[I "lambda", I "x", [I "lambda", I "y", [I "plus", I "x", I "y"]]], N 3], N 5] "")
    --   -- runParser parseSExpr "(   extra   (  spaces   ) )" `shouldBe` Just ([I "extra", [I "spaces"]], "")
