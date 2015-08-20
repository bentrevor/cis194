module Chapter01Spec where

import Chapter01
import Test.Hspec

spec :: Spec
spec = do
  describe "chapter 1" $ do
    it "toDigits" $ do
      toDigits (-123) `shouldBe` []
      toDigits 123 `shouldBe` [1, 2, 3]

    it "doubleEveryOther (from right)" $ do
      doubleEveryOther [] `shouldBe` []
      doubleEveryOther [1, 1, 2, 2, 3, 3] `shouldBe` [2, 1, 4, 2, 6, 3]

    it "sumDigits" $ do
      sumDigits [] `shouldBe` 0
      sumDigits [123, 456] `shouldBe` 21

    it "validate" $ do
      validate 4012888888881881 `shouldBe` True
      validate 4012888888881880 `shouldBe` False

    it "hanoi" $ do
      hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]
      hanoi 3 "a" "b" "c" `shouldBe` [("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b")]
