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

notFollowedByAlpha :: Parser a -> Parser a
notFollowedByAlpha pa = pa <* (satisfy (not . isAlpha))

type Ident = String

data Atom = N Integer | I Ident
          deriving (Show, Eq)

data SExpr = A Atom | Comb [SExpr]
           deriving (Show, Eq)

parseIdent :: Parser Atom
parseIdent = I <$> ident

-- int should only be followed by space, ')', or end of string
parseInt :: Parser Atom
parseInt = N <$> (notFollowedByAlpha posInt)

parseAtom :: Parser Atom
parseAtom = parseIdent <|> parseInt

-- parseSExpr :: Parser SExpr
-- parseSExpr = fmap A parseAtom
