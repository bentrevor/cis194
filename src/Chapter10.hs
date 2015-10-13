module Chapter10 where

import Control.Applicative
import Data.Char

data Employee = Emp { age :: Integer, name :: String } deriving (Eq, Show)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where f [] = Nothing
        f (x:xs)
          | p x       = Just (x, xs)
          | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where f xs
          | null ns   = Nothing
          | otherwise = Just (read ns, rest)
          where (ns, rest) = span isDigit xs

first :: (a -> b) -> (a, c) -> (b, c)
first fn (a, c) = (fn a, c)

instance Functor Parser where
  fmap fn (Parser pa) = Parser f
    where f str = fmap (first fn) (pa str)
                  -- case pa str of
                  --  Nothing -> Nothing
                  --  Just (parsed, rst) -> Just (fn parsed, rst)

instance Applicative Parser where
  pure s = Parser (\str -> Just (s, str))

  (Parser pf) <*> (Parser pa) = Parser pb
    where pb str = case pf str of
                    Nothing -> Nothing
                    Just (fn, restStr) -> fmap (first fn) (pa restStr)

parseAge :: Parser Integer
parseAge = posInt

parseName :: Parser String
parseName = Parser f
  where f xs
          | xs == []  = Nothing
          | otherwise = Just (xs, "")

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

xyParser :: Parser (Char, Char)
xyParser = (,) <$> char 'x' <*> char 'y'

intPair :: Parser [Integer]
intPair = (\a _ b -> [a,b]) <$> posInt <*> char ' ' <*> posInt

instance Alternative Parser where
  empty = Parser (\_ -> Nothing)
  p1 <|> p2 = Parser f
    where f str = runParser p1 str <|> runParser p2 str

intOrUppercase :: Parser ()
intOrUppercase = (\_ -> ()) <$> satisfy isUpper <|> (\_ -> () ) <$> posInt
