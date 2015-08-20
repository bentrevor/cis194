module Chapter04Spec where

import Chapter04
import Test.Hspec

spec :: Spec
spec = do
  describe "chapter 4" $ do
    it "fun1'" $ do
      fun1' [] `shouldBe` 1
      fun1' [2,3,4,5] `shouldBe` 0
      fun1' [3,4] `shouldBe` 2
      fun1' [3,4,5,6] `shouldBe` 8

    it "hailstone" $ do
      hailstone 1 `shouldBe` [1]
      hailstone 2 `shouldBe` [2,1]
      hailstone 3 `shouldBe` [3,10,5,16,8,4,2,1]

    it "fun2'" $ do
      fun2' 1 `shouldBe` 0
      fun2' 2 `shouldBe` 2
      fun2' 3 `shouldBe` fun2' 10
      fun2' 10 `shouldBe` 10 + fun2' 5

    it "height" $ do
      height (HNode 0 HLeaf 50 HLeaf) `shouldBe` 0
      height (foldTree "ABCDEFGHIJ") `shouldBe` 3

    it "countNodes" $ do
      countNodes HLeaf `shouldBe` 0
      countNodes (HNode 0 HLeaf 50 HLeaf) `shouldBe` 1
      countNodes (foldTree "ABCDEFGHIJ") `shouldBe` 10

    describe "hInsert" $ do
      it "creates a node when inserting into a leaf" $ do
        hInsert 1 HLeaf `shouldBe` HNode 0 HLeaf 1 HLeaf

      it "inserts into the subtree with fewer elements" $ do
        hInsert 'b' (HNode 0 HLeaf 'a' HLeaf) `shouldBe` HNode 1 (HNode 0 HLeaf 'b' HLeaf) 'a' HLeaf
        hInsert 'c' (HNode 1 HLeaf 'a' (HNode 0 HLeaf 'b' HLeaf)) `shouldBe` HNode 1 (HNode 0 HLeaf 'c' HLeaf) 'a' (HNode 0 HLeaf 'b' HLeaf)
        hInsert 'c' (HNode 1 (HNode 0 HLeaf 'b' HLeaf) 'a' HLeaf) `shouldBe` HNode 1 (HNode 0 HLeaf 'b' HLeaf) 'a' (HNode 0 HLeaf 'c' HLeaf)

      it "updates the height of a node" $ do
        hInsert 'd' (HNode 1 (HNode 0 HLeaf 'c' HLeaf) 'a' (HNode 0 HLeaf 'b' HLeaf)) `shouldBe` (HNode 2 (HNode 1 (HNode 0 HLeaf 'd' HLeaf) 'c' HLeaf) 'a' (HNode 0 HLeaf 'b' HLeaf))

    it "foldTree" $ do
      foldTree ([] :: [Int]) `shouldBe` HLeaf
      foldTree [1] `shouldBe` (HNode 0 HLeaf 1 HLeaf)
      foldTree [1,2] `shouldBe` (HNode 1 (HNode 0 HLeaf 1 HLeaf) 2 HLeaf)
      height (foldTree "ABCDEFGHIJ") `shouldBe` 3

    describe "oddTrues" $ do
      it "is True when there are an even number of Trues in the list" $ do
        oddTrues [] `shouldBe` True

      it "is True when there are an even number of Trues in the list" $ do
        oddTrues [True, True, False] `shouldBe` True

      it "is False when there are an odd number of Trues in the list" $ do
        oddTrues [True] `shouldBe` False

      it "is False when there are an odd number of Trues in the list" $ do
        oddTrues [True, True, True, False] `shouldBe` False

    describe "map'" $ do
      it "is the same as map" $ do
        map' (+5) [1..5] `shouldBe` [6..10]

    describe "sieveSundaram" $ do
      it "finds primes" $ do
        sieveSundaram 23 `shouldBe` [3,5,7,11,13,17,19,23,29,31,37,41,43,47]
