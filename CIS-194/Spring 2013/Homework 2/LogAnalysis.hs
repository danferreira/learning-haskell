{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
import Log

-- Exercise 1

parseMessage :: String -> LogMessage
parseMessage m = case words m of
  ("I":ts:ms) -> LogMessage Info (read ts) (unwords ms)
  ("W":ts:ms) -> LogMessage Warning (read ts) (unwords ms)
  ("E":s:ts:ms) -> LogMessage (Error (read s)) (read ts) (unwords ms)
  _ -> Unknown m

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm Leaf = Node Leaf lm Leaf
insert lm (Node left m right)
  | getTimeStamp lm <= getTimeStamp m = Node (insert lm left) m right
  | otherwise = Node left m (insert lm right)

-- Exercise 3

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf =  []
inOrder (Node left val right) = inOrder left ++ [val] ++ inOrder right

-- Exercise 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extractLogMessage . inOrder . build . filter(\x -> isError x && isSevere x)

-- Helpers

getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ ts _) = ts
getTimeStamp _ = error "Invalid message"

isError :: LogMessage -> Bool
isError (LogMessage (Error _) _ _) = True
isError _ = False

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error s) _ _) = (s >= 50)
isSevere _ = False

extractLogMessage :: LogMessage -> String
extractLogMessage (LogMessage _ _ m) = m
extractLogMessage (Unknown m) = m
