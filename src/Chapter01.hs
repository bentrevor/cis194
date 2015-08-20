module Chapter01 where

import Data.Char

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = map (toInteger . digitToInt) $ show n

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = (reverse . toDigits) n

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft [x] = [x]
doubleEveryOtherFromLeft [x,y] = [x, 2 * y]
doubleEveryOtherFromLeft (x:y:xs) = x : (2 * y) : doubleEveryOtherFromLeft xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ doubleEveryOtherFromLeft $ reverse xs

sumDigitsForNumber :: Integer -> Integer
sumDigitsForNumber n = (sum . toDigits) n

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ map sumDigitsForNumber xs

validate :: Integer -> Bool
validate num = digitSum `mod` 10 == 0
  where digitSum = sumDigits $ doubleEveryOther $ toDigits num

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++
                hanoi 1 a b c ++
                hanoi (n - 1) c b a
