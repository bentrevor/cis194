{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chapter07 where

import Sized

newtype Sum a = Sum a deriving (Eq, Ord, Num, Show)
newtype Product a = Product a deriving (Eq, Ord, Num, Show)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend = (+)

instance Num a => Monoid (Product a) where
  mempty = Product 1
  mappend = (*)

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

instance Sized (JoinList m a) where
  size Empty = 0
  size (Single _ _) = 1
  size (Append _ l r) = 1 + (size l) + (size r)

instance Sized (Product a) where
  size _ = 1

data Score a = Score a deriving (Eq, Show)

instance Num a => Monoid (Score a) where
  mempty = Score 0
  mappend (Score s1) (Score s2)= Score (s1 + s2)

score :: Char -> Score Int
score c = Score (valueForChar c)

scoreString :: String -> Score Int
scoreString str = Score (sum $ map valueForChar str)

scoreLine :: String -> JoinList (Score Int) String
scoreLine str = Single (scoreString str) str

valueForChar :: Char -> Int
valueForChar c = case c of
                  'e' -> 1
                  'a' -> 1
                  'i' -> 1
                  'o' -> 1
                  'n' -> 1
                  'r' -> 1
                  't' -> 1
                  'l' -> 1
                  's' -> 1
                  'u' -> 1
                  'd' -> 2
                  'g' -> 2
                  'b' -> 3
                  'c' -> 3
                  'm' -> 3
                  'p' -> 3
                  'f' -> 4
                  'h' -> 4
                  'v' -> 4
                  'w' -> 4
                  'y' -> 4
                  'k' -> 5
                  'j' -> 8
                  'x' -> 8
                  'q' -> 10
                  'z' -> 10
                  _ -> 0

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) listA listB = Append (mappend (tag listA) (tag listB)) listA listB

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_)  !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ x) = [x]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ i xs = (jlToList xs) !!? i

leaves :: JoinList m a -> Int
leaves Empty = 0
leaves (Single _ _) = 1
leaves (Append _ l1 l2) = leaves l1 + leaves l2

dropJ :: Int -> (JoinList b a) -> (JoinList b a)
dropJ 0 jl = jl
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ 1 (Append m l1 l2)
  | leaves l1 == 0 = Append m l1 (dropJ 1 l2)
  | otherwise      = Append m (dropJ 1 l1) l2
dropJ i jl = dropJ (i - 1) (dropJ 1 jl)

takeJ :: Monoid b => Int -> (JoinList b a) -> (JoinList b a)
takeJ 0 _ = Empty
takeJ _ Empty = Empty
takeJ _ s@(Single _ _) = s
takeJ n (Append _ left right)
  | leaves left >= n = takeJ n left
  | otherwise        = left +++ (takeJ remaining right)
  where remaining = n - (leaves left)
