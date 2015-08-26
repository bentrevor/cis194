module Chapter05Spec where

import Chapter05
import StackVM
import Test.Hspec

spec :: Spec
spec = do
  describe "chapter 5" $ do
    it "eval" $ do
      eval (Mul' (Add' (Lit' 2) (Lit' 3)) (Lit' 4)) `shouldBe` 20

    it "evalStr" $ do
      evalStr "(2+3)*4" `shouldBe` Just 20
      evalStr "2+3*4" `shouldBe` Just 14
      evalStr "(2+3)*" `shouldBe` Nothing

    it "Expr" $ do
      ((mul (add (lit 2) (lit 3)) (lit 4)) :: ExprT) `shouldBe` Mul' (Add' (Lit' 2) (Lit' 3)) (Lit' 4)

    it "Integer" $ do
      ((lit 3) :: Integer) `shouldBe` 3
      ((lit (-3)) :: Integer) `shouldBe` (-3)
      (add (lit 3) (lit (-3)) :: Integer) `shouldBe` 0
      (add (lit (-4)) (lit (-3)) :: Integer) `shouldBe` (-7)
      (mul (lit 3) (lit (-3)) :: Integer) `shouldBe` (-9)
      (mul (lit (-4)) (lit (-3)) :: Integer) `shouldBe` 12
      (mul (lit 3) (lit 4) :: Integer) `shouldBe` 12

    it "Bool" $ do
      ((lit 3) :: Bool) `shouldBe` True
      ((lit (-3)) :: Bool) `shouldBe` False
      (add (lit 3) (lit (-3)) :: Bool) `shouldBe` True
      (add (lit (-4)) (lit (-3)) :: Bool) `shouldBe` False
      (mul (lit 3) (lit (-3)) :: Bool) `shouldBe` False
      (mul (lit (-4)) (lit (-3)) :: Bool) `shouldBe` False
      (mul (lit 3) (lit 4) :: Bool) `shouldBe` True

    it "MinMax" $ do
      ((lit 3) :: MinMax) `shouldBe` MinMax 3
      ((lit (-3)) :: MinMax) `shouldBe` MinMax (-3)
      (add (lit 3) (lit (-3)) :: MinMax) `shouldBe` MinMax 3
      (add (lit (-4)) (lit (-3)) :: MinMax) `shouldBe` MinMax (-3)
      (mul (lit 3) (lit (-3)) :: MinMax) `shouldBe` MinMax (-3)
      (mul (lit (-4)) (lit (-3)) :: MinMax) `shouldBe` MinMax (-4)
      (mul (lit 3) (lit 4) :: MinMax) `shouldBe` MinMax 3

    it "Mod7" $ do
      ((lit 3) :: Mod7) `shouldBe` Mod7 3
      ((lit (-3)) :: Mod7) `shouldBe` Mod7 4
      (add (lit 3) (lit (-3)) :: Mod7) `shouldBe` Mod7 0
      (add (lit (-4)) (lit (-3)) :: Mod7) `shouldBe` Mod7 0
      (mul (lit 3) (lit (-3)) :: Mod7) `shouldBe` Mod7 5
      (mul (lit (-4)) (lit (-3)) :: Mod7) `shouldBe` Mod7 5
      (mul (lit 3) (lit 4) :: Mod7) `shouldBe` Mod7 5

    -- it "Program" $ do
    --   ((lit 3) :: Program) `shouldBe` [PushI 3]
    --   ((lit (-3)) :: Program) `shouldBe` [PushI (-3)]
    --   (add (lit 3) (lit (-3)) :: Program) `shouldBe` [PushI (-3), PushI 3, Add]
