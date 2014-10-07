{-# OPTIONS_GHC -Wall #-}
module Histogram where
import Data.List
histogram :: [Integer] -> String
histogram xs = (concat . map (++ "\n") $ transpose $ map pad groups) 
    ++ "==========\n0123456789\n"
    where
    groups = map tail $ group $ sort $ [0..9] ++ xs
    height = maximum $ map length groups
    pad ys = replicate (height - length ys) ' ' ++ replicate (length ys) '*' 
