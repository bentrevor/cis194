module Chapter10Spec where

import Chapter10
import Control.Applicative
import Data.Char
import Test.Hspec

spec :: Spec
spec = do
  describe "chapter 10" $ do
    it "first" $ do
      first (+1) (5, "asdf") `shouldBe` (6, "asdf")

    it "instance Functor" $ do
      runParser (fmap (+1) posInt) "123asdf" `shouldBe` Just (124, "asdf")

    it "instance Applicative" $ do
      runParser (pure 5) "asdf" `shouldBe` Just (5, "asdf")
      runParser (pure toUpper <*> char 'a') "asdf" `shouldBe` Just ('A', "sdf")
      runParser (Emp <$> parseAge <*> parseName) "27asdf" `shouldBe` Just (Emp { age = 27, name = "asdf"}, "")

    it "abParser" $ do
      runParser abParser "abcdef" `shouldBe` Just (('a', 'b'), "cdef")
      runParser abParser "acdef" `shouldBe` Nothing

    it "abParser_" $ do
      runParser abParser_ "abcdef" `shouldBe` Just ((), "cdef")
      runParser abParser_ "acdef" `shouldBe` Nothing

    it "intPair" $ do
      runParser intPair "12 34" `shouldBe` Just ([12, 34], "")

    it "instance Alternative" $ do
      runParser (empty :: Parser ()) "asdf" `shouldBe` Nothing
      runParser (empty <|> xyParser) "xyzw" `shouldBe` Just (('x','y'), "zw")
      runParser (xyParser <|> empty) "xyzw" `shouldBe` Just (('x','y'), "zw")
      runParser (xyParser <|> xyParser) "xyzw" `shouldBe` Just (('x','y'), "zw")
      runParser (abParser <|> xyParser) "xyzw" `shouldBe` Just (('x','y'), "zw")
      runParser (abParser <|> xyParser) "abcd" `shouldBe` Just (('a','b'), "cd")
      runParser (abParser <|> xyParser) "12 34" `shouldBe` Nothing

    it "intOrUppercase" $ do
      runParser intOrUppercase "342asdf" `shouldBe` Just ((), "asdf")
      runParser intOrUppercase "XYZ" `shouldBe` Just ((), "YZ")
      runParser intOrUppercase "qwer" `shouldBe` Nothing
