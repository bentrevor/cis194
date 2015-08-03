module CS194 where

import Data.Char
import Data.List

import Log

---------------
-- chapter 1 --
---------------
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

---------------
-- chapter 2 --
---------------
notSpace :: Char -> Bool
notSpace = (/=' ')

firstWord :: String -> String
firstWord = takeWhile notSpace

readIntWord :: String -> Int
readIntWord s = read $ (firstWord s) :: Int

dropFirstWord :: String -> String
dropFirstWord s = tail $ dropWhile notSpace s

parseMessageType :: String -> (MessageType, String)
parseMessageType ('I':' ':cs) = (Info, cs)
parseMessageType ('W':' ':cs) = (Warning, cs)
parseMessageType ('E':' ':cs) = (Error (readIntWord cs), dropFirstWord cs)
parseMessageType _ = (Error (-1), "parseMessageType was called with an invalid log message")

parseTimestamp :: String -> (Int, String)
parseTimestamp s = (readIntWord s, dropFirstWord s)

parseLogMessage :: String -> LogMessage
parseLogMessage logEntry = LogMessage messageType timestamp message
  where (messageType, entryAfterMsgType) = parseMessageType logEntry
        (timestamp, message)             = parseTimestamp entryAfterMsgType

getTimestamp :: LogMessage -> TimeStamp
getTimestamp (Unknown _) = (-1)
getTimestamp (LogMessage _ timestamp _) = timestamp

getMessage :: LogMessage -> String
getMessage (Unknown msg) = msg
getMessage (LogMessage _ _ msg) = msg

parseMessage :: String -> LogMessage
parseMessage s
  | elem (take 2 s) ["I ", "W ", "E "] = parseLogMessage s
  | otherwise                          = Unknown s

parse :: String -> [LogMessage]
parse fileContents = map parseMessage $ lines fileContents

rootVal :: MessageTree -> LogMessage
rootVal Leaf = Unknown ""
rootVal (Node _ msg _) = msg

makeNode :: LogMessage -> MessageTree
makeNode msg = Node Leaf msg Leaf

left :: MessageTree -> MessageTree
left Leaf = Leaf
left (Node l _ _) = l

right :: MessageTree -> MessageTree
right Leaf = Leaf
right (Node _ _ r) = r

insert' :: LogMessage -> MessageTree -> MessageTree
insert' msg Leaf = Node Leaf msg Leaf
insert' (Unknown _) tree = tree
insert' msg tree
  | (getTimestamp msg) <= (getTimestamp $ rootVal tree) = Node (insert' msg (left tree)) (rootVal tree) (right tree)
  | otherwise                            = Node (left tree) (rootVal tree) (insert' msg (right tree))

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (msg:msgs) = insert' msg $ build msgs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l msg r) = (inOrder l) ++ [msg] ++ (inOrder r)

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = inOrder . build

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error n) _ _)
  | n > 50 = True
  | otherwise = False
isSevereError _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong msgs = map getMessage $ filter isSevereError $ sortMessages msgs

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
