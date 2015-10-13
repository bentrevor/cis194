{-# OPTIONS_GHC -fno-warn-orphans #-}

module Chapter08 where

import Employee
import Data.Tree

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL guests _) gl2 = foldr glCons gl2 guests

instance Show GuestList where
  show (GL emps fun) = "Total fun: " ++ show fun ++ "\n" ++ empNames
    where empNames = unlines $ map empName emps

glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp _ fun) (GL guests totalFun) = GL (emp : guests) (totalFun + fun)

-- GuestList implements Ord by comparing fun values
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- a == Employee
-- b == (GuestList, GuestList)
-- fn will be nextLevel
treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold fn guestLists (Node {rootLabel = emp, subForest = emps}) = fn emp $ map (treeFold fn guestLists) emps

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss results = (bestWithBoss, bestWithoutBoss)
  where bestWithBoss = glCons boss guestListWithoutBosses
        guestListWithoutBosses = mconcat $ map snd results
        bestWithoutBoss = mconcat $ map fst results

maxFun :: Tree Employee -> GuestList
maxFun tree = betterList $ treeFold nextLevel mempty tree
  where betterList (gl1, gl2) = moreFun gl1 gl2

printAnswer :: IO ()
printAnswer = readFile "company.txt" >>= (\c -> putStrLn $ (show . maxFun . read) c)
