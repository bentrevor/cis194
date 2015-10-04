module Chapter08Spec where

import Chapter08
import Data.Monoid
import Employee
import Data.Tree
import Test.Hspec


companyWithBoss :: Employee -> Tree Employee
companyWithBoss boss = Node boss
                       [ Node (Emp "Bob" 2)
                         [ Node (Emp "Joe" 5)
                           [ Node (Emp "John" 1) []
                           , Node (Emp "Sue" 5) []
                           ]
                         , Node (Emp "Fred" 3) []
                         ]
                       , Node (Emp "Sarah" 17)
                         [ Node (Emp "Sam" 4) []
                         ]
                       ]

--            boss
--            /  \
--           2    17
--          / \    \
--         5   3    4
--        / \
--       1   5


boringBoss = Emp "boring" 2
funBoss = Emp "fun" 2000

gl1 = GL [(Emp "Asdf" 5)] 5
gl2 = GL [(Emp "Asdf" 5), (Emp "Qwer" 8)] 13

emp1 = Emp "emp1" 1
emp2 = Emp "emp2" 2
emp5 = Emp "emp5" 5

spec :: Spec
spec = do
  describe "chapter 8" $ do
    it "glCons" $ do
      glCons (Emp "Asdf" 5) mempty `shouldBe` GL [(Emp "Asdf" 5)] 5
      glCons (Emp "Asdf" 5) (GL [(Emp "Qwer" 8)] 8) `shouldBe` GL [(Emp "Asdf" 5), (Emp "Qwer" 8)] 13

    it "instance Monoid" $ do
      (GL [(Emp "Asdf" 5)] 5) <> mempty `shouldBe` GL [(Emp "Asdf" 5)] 5
      (GL [(Emp "Asdf" 5)] 5) <> (GL [(Emp "Qwer" 7)] 7) `shouldBe` GL [(Emp "Asdf" 5), (Emp "Qwer" 7)] 12

    it "moreFun" $ do
      moreFun mempty mempty `shouldBe` mempty
      moreFun gl1 mempty `shouldBe` gl1
      moreFun gl1 gl2 `shouldBe` gl2

    it "treeFold" $ do
      treeFold nextLevel (mempty, mempty) (Node emp1 []) `shouldBe` (GL [emp1] 1, mempty)
      -- 1 -> 5 -> 2
      treeFold nextLevel ((GL [emp5] 5), (GL [emp2] 2)) (Node emp1 [Node emp5 [Node emp2 []]]) `shouldBe` ((GL [emp1, emp2] 3), (GL [emp5] 5))

    it "nextLevel" $ do
      -- 5 -> 1 -> 2
      nextLevel emp5 [(GL [emp1] 1, GL [emp2] 2)] `shouldBe` (GL [emp5, emp2] 7, GL [emp1] 1)
      -- 2 -> 1 -> 5
      nextLevel emp2 [(GL [emp1] 1, GL [emp5] 5)] `shouldBe` (GL [emp2, emp5] 7, GL [emp1] 1)
