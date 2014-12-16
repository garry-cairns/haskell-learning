{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage _ = Unknown "Hi"

parse :: String -> [LogMessage]

main :: IO()
main = do
    print $ parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
    print $ parseMessage "This is not the right format" == Unknown "This is not the right format"
