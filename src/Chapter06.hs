module Chapter06 where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs' :: [Integer]
fibs' = map fib [0..]

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x rest) = [x] ++ streamToList rest

instance Show a => Show (Stream a) where
  show stream = "stream: " ++ show stream

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn (Stream x rest) = Stream (fn x) (streamMap fn rest)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fn seed = Stream seed (streamFromSeed fn (fn seed))

nats :: Stream Integer
nats = streamFromSeed (+1) 1

factorsOf2 :: Integer -> Integer
factorsOf2 n
  | n `mod` 2 == 1 = 0
  | otherwise      = 1 + factorsOf2 (n `div` 2)

ruler :: Stream Integer
ruler = streamMap factorsOf2 nats
