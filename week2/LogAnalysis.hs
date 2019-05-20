{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- EXERCISE 1 --

parseMessage :: String -> LogMessage
parseMessage = 
  convert . words
  where convert :: [String] -> LogMessage
        convert ("I":t:m) = LogMessage Info (read t) (unwords m)
        convert ("W":t:m) = LogMessage Warning (read t) (unwords m)
        convert ("E":c:t:m) = LogMessage (Error (read c)) (read t) (unwords m)
        convert x = Unknown (unwords x)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- EXERCISE 2 --

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert newLog Leaf = Node Leaf newLog Leaf
insert newLog (Node left root right) = 
  let t1 = getTime newLog
      t2 = getTime root
  in case compare t1 t2 of
    LT  -> Node (insert newLog left) root right
    _   -> Node left root (insert newLog right)
  where getTime :: LogMessage -> TimeStamp
        getTime (LogMessage _ t _) = t
        getTime _ = 0

-- EXERCISE 3 --

build :: [LogMessage] ->  MessageTree
-- build [] = Leaf
-- build (x:xs) = insert x (build xs)
build = foldr insert Leaf

-- EXERCISE 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left (Unknown _) right) = inOrder(left) ++ inOrder(right)
inOrder (Node left message right) = inOrder(left) ++ [message] ++ inOrder(right) 

-- EXERCISE 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong  = map (\(LogMessage _ t m) -> (show t) ++ m) . inOrder . build . filter isRelevant 
    where 
      isRelevant :: LogMessage -> Bool
      isRelevant (LogMessage (Error e) _ _) = e >= 50
      isRelevant _ = False