module Chapter03 where

import Data.List

everyNth :: Int -> [a] -> [a]
everyNth _ [] = []
everyNth 1 xs = xs
everyNth n xs
  | n > length xs = []
  | otherwise      = (xs !! (n - 1)) : (everyNth n (drop n xs))

skips :: Eq a => [a] -> [[a]]
skips [] = []
skips xs = map (\n -> everyNth n xs) [1..(length xs)]

isLocalMaximaIndex :: [Integer] -> Int -> Bool
isLocalMaximaIndex xs i = (xs !! i) > (xs !! (i - 1)) && (xs !! i) > (xs !! (i + 1))

localMaxima :: [Integer] -> [Integer]
localMaxima xs
  | length xs < 3 = []
  | otherwise     = map (xs!!) maxIndexes
  where maxIndexes = filter (isLocalMaximaIndex xs) indexRange
        indexRange = [1..((length xs) - 2)]

nTimes :: Int -> a -> [a]
nTimes n val = map (\_ -> val) [1..n]

maxStars :: [Integer] -> Int
maxStars xs = maximum $ map length $ (group . sort) xs

starsForIndex :: [Integer] -> Integer -> Int
starsForIndex xs i = length $ filter (==i) xs

columnForIndex :: [Integer] -> Integer -> String
columnForIndex xs i = nTimes numSpaces ' ' ++ nTimes numStars '*'
  where numStars = starsForIndex xs i
        numSpaces = (maxStars xs) - numStars

histogram :: [Integer] -> String
histogram ints = starRows ++ labels
  where labels = "==========\n0123456789\n"
        starRows = unlines rows
        rows = transpose columns
        columns = map (columnForIndex ints) [0..9]
