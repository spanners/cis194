{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

{- TODO 
Could use serious cleaning up with
`take`, `drop`, (.)
-}

parseMessage :: String -> LogMessage
parseMessage m = parseHelper $ words m

parseHelper :: [String] -> LogMessage
parseHelper wrds = case (head wrds) of
               "I" -> LogMessage Info (bar wrds) (baz wrds)
               "W" -> LogMessage Warning (bar wrds) (baz wrds)
               "E" -> LogMessage (Error $ bar wrds) (bar $ tail wrds) (baz $ tail wrds)
               _   -> Unknown $ unwords wrds

bar :: [String] -> Int
bar = read . head . tail

baz :: [String] -> String
baz = unwords . tail . tail

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node l lm r) | getLevel msg < getLevel lm  = Node (insert msg l) lm r
                         | getLevel msg >= getLevel lm = Node l lm (insert msg r)

getLevel :: LogMessage -> Int
getLevel (LogMessage _ l _) = l

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l lm r) = inOrder l ++ [lm] ++ inOrder r
