module Chapter11 where

import AParser
import Data.Char
import Control.Applicative

-- run p once and then run zeroOrMore
oneOrMore :: Parser a -> Parser [a]
oneOrMore pa = (:) <$> pa <*> zeroOrMore pa

-- try parsing oneOrMore, otherwise return empty list
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore pa = oneOrMore pa <|> pure []

spaces :: Parser String
spaces = zeroOrMore $ char ' '

ident :: Parser String
ident = (++) <$> parseAlphas <*> maybeParseAlphaNums
  where parseAlphas = oneOrMore $ satisfy isAlpha
        maybeParseAlphaNums = zeroOrMore $ satisfy isAlphaNum

-- int should only be followed by space, ')', or end of string
-- (but I think that's out of the scope of this hw...)
-- notFollowedByAlpha :: Parser a -> Parser a
-- notFollowedByAlpha pa = pa <* (satisfy (not . isAlpha))

type Ident = String

data Atom = N Integer | I Ident
          deriving (Show, Eq)

data SExpr = A Atom | Comb [SExpr]
           deriving (Show, Eq)

parseIdent :: Parser Atom
parseIdent = I <$> ident

parseInt :: Parser Atom
parseInt = N <$> posInt

parseAtom :: Parser Atom
parseAtom = parseInt <|> parseIdent

parseA :: Parser SExpr
parseA = A <$> parseAtom

parseComb :: Parser SExpr
parseComb = char '(' *> parseComb' <* char ')'
  where parseComb' = Comb <$> oneOrMore parseSExpr

parseSExpr :: Parser SExpr
parseSExpr = spaces *> parseSExpr' <* spaces
  where parseSExpr' = parseA <|> parseComb
