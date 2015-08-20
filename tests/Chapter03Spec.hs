module Chapter03Spec where

import Chapter03
import Test.Hspec

spec :: Spec
spec = do
  describe "chapter 3" $ do
    it "everyNth" $ do
      everyNth 2 "ABCD" `shouldBe` "BD"
      everyNth 1 "hello!" `shouldBe` "hello!"
      everyNth 2 "hello!" `shouldBe` "el!"
      everyNth 3 "hello!" `shouldBe` "l!"

    it "skips" $ do
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
      skips [1] `shouldBe` [[1]]
      skips [True,False] `shouldBe` [[True,False], [False]]
      skips ([] :: [Int]) `shouldBe` []

    it "localMaxima" $ do
      localMaxima [2,9,5,6,1] `shouldBe` [9,6]
      localMaxima [2,3,4,1,5] `shouldBe` [4]
      localMaxima [1,2,3,4,5] `shouldBe` []

    it "nTimes" $ do
      nTimes 5 'x' `shouldBe` "xxxxx"

    it "maxStars" $ do
      maxStars [1,4,5,4,6,6,3,4,2,4,9] `shouldBe` 4

    it "starsForIndex" $ do
      starsForIndex [1,4,5,4,6,6,3,4,2,4,9] 4 `shouldBe` 4
      starsForIndex [1,4,5,4,6,6,3,4,2,4,9] 7 `shouldBe` 0

    it "columnForIndex" $ do
      columnForIndex [1,4,5,4,6,6,3,4,2,4,9] 4 `shouldBe` "****"
      columnForIndex [1,4,5,4,6,6,3,4,2,4,9] 6 `shouldBe` "  **"
      columnForIndex [1,4,5,4,6,6,3,4,2,4,9] 7 `shouldBe` "    "

--     it "histogram" $ do
--       histogram [3, 5] `shouldBe` "   * *    \n==========\n0123456789\n"
--       histogram [1, 1, 1, 5] `shouldBe` "\
-- \ *        \n\
-- \ *        \n\
-- \ *   *    \n\
-- \==========\n\
-- \0123456789\n"
--       histogram [1,4,5,4,6,6,3,4,2,4,9] `shouldBe` "\
-- \    *     \n\
-- \    *     \n\
-- \    * *   \n\
-- \ ******  *\n\
-- \==========\n\
-- \0123456789\n"
