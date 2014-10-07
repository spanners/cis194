{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage m = parseHelper $ words m

parseHelper :: [String] -> LogMessage
parseHelper wrds = case (head wrds) of
    "I" -> LogMessage Info               (lvl wrds)        (msg wrds)
    "W" -> LogMessage Warning            (lvl wrds)        (msg wrds)
    "E" -> LogMessage (Error $ lvl wrds) (lvl $ tail wrds) (msg $ tail wrds)
    _   -> Unknown $ unwords wrds
  where lvl :: [String] -> Int
        lvl = read . head . tail
        msg :: [String] -> String
        msg = unwords . drop 2

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _)         t        = t
insert msg                 Leaf     = Node Leaf msg Leaf
insert msg                (Node l lm r) 
    | getLevel msg < getLevel lm    = Node (insert msg l) lm r
    | getLevel msg >= getLevel lm   = Node l lm (insert msg r)
insert (LogMessage _ _ _) (Node _ _  _)
                                    = error "We should not get here"

getLevel :: LogMessage -> Int
getLevel (LogMessage _ l _) = l
getLevel (Unknown _)        = error "LogMessage Unknown has no level"

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf          = []
inOrder (Node l lm r) = inOrder l ++ [lm] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map show . filter ((>50) . getLevel) 
