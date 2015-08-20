module Chapter04 where

data BalTree a = HLeaf
               | HNode Integer (BalTree a) a (BalTree a)
               deriving (Show, Eq)

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

hailstone :: Integer -> [Integer]
hailstone 1 = [1]
hailstone n = [n] ++ hailstone (next n)
  where next m
          | even m    = m `div` 2
          | otherwise = m * 3 + 1

fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' x = sum . filter even $ hailstones
  where hailstones = takeWhile (/=1) $ hailstone x

height :: BalTree a -> Integer
height HLeaf = 0
height (HNode h _ _ _) = h

countNodes :: BalTree a -> Int
countNodes HLeaf = 0
countNodes (HNode _ l _ r) = 1 + (countNodes l) + (countNodes r)

isBalanced :: BalTree a -> Bool
isBalanced HLeaf = True
isBalanced (HNode _ l _ r) = isBalanced l && isBalanced r && countNodes r == countNodes l

hInsert :: (Ord a, Eq a) => a -> BalTree a -> BalTree a
hInsert x HLeaf = HNode 0 HLeaf x HLeaf
hInsert x t@(HNode _ l val r)
  | countNodes l <= countNodes r = HNode newHeight (hInsert x l) val r
  | otherwise                    = HNode newHeight l val (hInsert x r)
  where newHeight
          | isBalanced t = (height t) + 1
          | otherwise    = (height t)

foldTree :: (Ord a, Eq a) => [a] -> BalTree a
foldTree [] = HLeaf
foldTree xs = foldr hInsert HLeaf xs

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

oddTrues :: [Bool] -> Bool
oddTrues = foldr xor True

map' :: (a -> b) -> [a] -> [b]
map' fn xs = foldr (\x y -> [fn x] ++ y) [] xs

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> (2*x) + 1) seeds
  where seeds = filter isSeed [1..n]
        isSeed x = not $ elem x nonSeeds
        nonSeeds = map weirdSum pairs
        weirdSum (i, j) = i + j + (2 * i * j)
        pairs = filter (\(x,y) -> x <= y) allPairs
        allPairs = [(a, b) | a <- [1..n], b <- [1..n]]
