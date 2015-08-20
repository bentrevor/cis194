module Chapter02 where

import Log

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
