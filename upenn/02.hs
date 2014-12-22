{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage xs
  | ws!!0 == "I" = LogMessage Info (read (ws!!1)) (unwords $ drop 2 ws)
  | ws!!0 == "W" = LogMessage Warning (read (ws!!1)) (unwords $ drop 2 ws)
  | ws!!0 == "E" = LogMessage (Error (read (ws!!1))) (read (ws!!2)) (unwords $ drop 2 ws)
  | otherwise = Unknown xs
  where ws = words xs

parse :: String -> [LogMessage]
parse xs = map parseMessage $ lines xs

insert :: LogMessage -> MessageTree -> MessageTree
insert _ _ = MessageTree

main :: IO()
main = do
    print $ parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
    print $ parseMessage "This is not the right format" == Unknown "This is not the right format"
