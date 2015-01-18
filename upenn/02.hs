{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage xs
  | head ws == "I" = LogMessage Info (read (ws!!1)) (unwords $ drop 2 ws)
  | head ws == "W" = LogMessage Warning (read (ws!!1)) (unwords $ drop 2 ws)
  | head ws == "E" = LogMessage (Error (read (ws!!1))) (read (ws!!2)) (unwords $ drop 2 ws)
  | otherwise = Unknown xs
  where ws = words xs

parse :: String -> [LogMessage]
parse xs = map parseMessage $ lines xs

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert x Leaf = Node Leaf x Leaf
insert x@(LogMessage _ timestamp _) (Node leftChild y@(LogMessage _ u _) rightChild)
  | timestamp > u = Node leftChild y (insert x rightChild)
  | otherwise = Node (insert x leftChild) y rightChild

build :: [LogMessage] -> MessageTree
build (x:xs) = insert x (build xs)
build [] = Leaf
--
inOrder :: MessageTree -> [LogMessage]
inOrder (Node leftChild@(Node _ _ _) _ _) = inOrder leftChild
inOrder (Node Leaf x rightChild@(Node _ _ _)) = x:inOrder rightChild
inOrder (Node Leaf x Leaf) = [x]
inOrder Leaf = []
--
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong (l@(LogMessage (Error n) _ s):ls)
  | n > 50 = s:whatWentWrong ls
  | otherwise = whatWentWrong ls
whatWentWrong (l:ls) = whatWentWrong ls
whatWentWrong [] = []
