module Chapter06Spec where

import Chapter06
import Test.Hspec

zeros :: Stream Integer
zeros = streamRepeat 0

spec :: Spec
spec = do
  describe "chapter 6" $ do
    it "fib" $ do
      fib 0 `shouldBe` 0
      fib 1 `shouldBe` 1
      fib 2 `shouldBe` 1
      fib 3 `shouldBe` 2
      fib 10 `shouldBe` 55

    it "fibs'" $ do
      take 10 fibs' `shouldBe` [0,1,1,2,3,5,8,13,21,34]

    it "streamRepeat" $ do
      take 5 (streamToList (streamRepeat 5)) `shouldBe` [5,5,5,5,5]

    it "streamToList" $ do
      take 5 (streamToList (Stream 1 zeros)) `shouldBe` [1, 0, 0, 0, 0]
      take 5 (streamToList (Stream 2 (Stream 1 zeros))) `shouldBe` [2, 1, 0, 0, 0]

    it "streamMap" $ do
      take 5 (streamToList (streamMap (+1) (streamRepeat 6))) `shouldBe` [7,7,7,7,7]

    it "streamFromSeed" $ do
      take 5 (streamToList (streamFromSeed (+1) 1)) `shouldBe` [1,2,3,4,5]

    it "nats" $ do
      take 5 (streamToList nats) `shouldBe` [1,2,3,4,5]

    it "ruler" $ do
      take 16 (streamToList ruler) `shouldBe` [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4]
