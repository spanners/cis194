{-# OPTIONS_GHC -Wall #-}
module Histogram where
import Data.List (intersperse, group, transpose, sort)
histogram :: [Integer] -> String
histogram xs = (concat $ intersperse "\n" $ transpose $ map pad groups) 
    ++ "\n==========\n0123456789\n"
    where
    groups = map tail $ group $ sort $ [0..9] ++ xs
    height = maximum $ map length groups
    pad ys = replicate (height - length ys) ' ' ++ replicate (length ys) '*' 
