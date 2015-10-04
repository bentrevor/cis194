module Chapter08 where

import Employee
import Data.Tree

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL guests _) gl2 = foldr glCons gl2 guests

glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp _ fun) (GL guests totalFun) = GL (emp : guests) (totalFun + fun)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- a == Employee
-- b == (GuestList, GuestList)
-- fn will be nextLevel
treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold fn guestLists (Node {rootLabel = emp, subForest = emps}) = fn emp $ map (treeFold fn guestLists) emps

glConcat :: [GuestList] -> GuestList
glConcat [] = mempty
glConcat (gl:gls) = foldr mappend gl gls

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss results = (bestWithBoss, bestWithoutBoss)
  where bestWithBoss = glCons boss guestListWithoutBosses
        guestListWithoutBosses = glConcat $ map snd results
        bestWithoutBoss = glConcat $ map fst results
