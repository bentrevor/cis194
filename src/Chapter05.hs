{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Chapter05 where

import Parser
import StackVM

data ExprT = Lit' Integer
           | Add' ExprT ExprT
           | Mul' ExprT ExprT
           deriving (Show, Eq)

newtype MinMax = MinMax Integer deriving (Show, Eq, Ord)
newtype Mod7 = Mod7 Integer deriving (Show, Eq)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit'
  add = Add'
  mul = Mul'

instance Expr Integer where
  lit n = n
  add a b = a + b
  mul a b = a * b

instance Expr Bool where
  lit n = if n <= 0 then False else True
  add a b = a || b
  mul a b = a && b

instance Expr MinMax where
  lit n = MinMax n
  add = max
  mul = min

instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

instance Expr Program where
  lit n = [PushI n]
  add p1 p2 = [Add] ++ p1 ++ p2
  mul p1 p2 = [Mul] ++ p1 ++ p2

eval :: ExprT -> Integer
eval (Lit' n) = n
eval (Add' expA expB) = (eval expA) + (eval expB)
eval (Mul' expA expB) = (eval expA) * (eval expB)

evalStr :: String -> Maybe Integer
evalStr str = fmap eval (parseExp Lit' Add' Mul' str)
