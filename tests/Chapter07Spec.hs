module Chapter07Spec where

import Chapter07
import Test.Hspec

leftList = (Append (Product 30)
            (Single (Product 5) 's')
            (Append (Product 6)
             (Single (Product 2) 'w')
             (Single (Product 3) 'a')))

rightList = Single (Product 7) 'g'

joinList :: JoinList (Product Int) Char
joinList = Append (Product (210 :: Int)) leftList rightList

spec :: Spec
spec = do
  describe "chapter 7" $ do
    it "tag" $ do
      tag Empty `shouldBe` Sum 0
      tag (Single (Sum 10) 'x') `shouldBe` Sum 10
      tag (Append (Sum 10) Empty Empty) `shouldBe` Sum 10

    it "(+++)" $ do
      tag ((Single (Sum 10) 'x') +++ (Single (Sum 15) 'y')) `shouldBe` (Sum 25)
      leftList +++ rightList `shouldBe` joinList

    it "jlToList" $ do
      jlToList joinList `shouldBe` "swag"

    it "indexJ" $ do
      indexJ 1 joinList `shouldBe` Just 'w'
      indexJ 400 joinList `shouldBe` Nothing

    it "dropJ" $ do
      jlToList (dropJ 0 joinList) `shouldBe` "swag"
      jlToList (dropJ 1 joinList) `shouldBe` "wag"
      jlToList (dropJ 2 joinList) `shouldBe` "ag"
      jlToList (dropJ 3 joinList) `shouldBe` "g"
      jlToList (dropJ 4 joinList) `shouldBe` ""

    it "takeJ" $ do
      takeJ 0 joinList `shouldBe` Empty
      takeJ 1 joinList `shouldBe` (Single (Product 5) 's')
      takeJ 2 joinList `shouldBe` Append (Product 10) (Single (Product 5) 's') (Single (Product 2) 'w')
      takeJ 4 joinList `shouldBe` joinList

      jlToList (takeJ 0 joinList) `shouldBe` ""
      jlToList (takeJ 1 joinList) `shouldBe` "s"
      jlToList (takeJ 2 joinList) `shouldBe` "sw"
      jlToList (takeJ 3 joinList) `shouldBe` "swa"
      jlToList (takeJ 4 joinList) `shouldBe` "swag"

    it "score" $ do
      score 'a' `shouldBe` Score 1
      score 'e' `shouldBe` Score 1
      score 'i' `shouldBe` Score 1
      score 'o' `shouldBe` Score 1
      score 'u' `shouldBe` Score 1
      score 'y' `shouldBe` Score 4
      score 'z' `shouldBe` Score 10

    it "scoreString" $ do
      scoreString "asdf" `shouldBe` Score 8
      scoreString "yolo" `shouldBe` Score 7

    it "scoreLine" $ do
      scoreLine "yay " `shouldBe` Single (Score 9) "yay "
      scoreLine "yay " +++ scoreLine "haskell!" `shouldBe` Append (Score 23) (Single (Score 9) "yay ") (Single (Score 14) "haskell!")
