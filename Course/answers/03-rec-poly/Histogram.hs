{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes #-}

module Histogram where

import Data.Map (toList, fromListWith)
import Data.List (maximumBy, transpose, intersperse)

histogram :: [Integer] -> String
histogram xs = helper xs ++ "\n=========\n123456789"

helper :: [Integer] -> String
helper = concat . addNewlines . transpose . conc . addPad

conc :: forall a. [([a], [a])] -> [[a]]
conc = map (\(a,b) -> a ++ b)

addPad :: [Integer] -> [(String, String)]
addPad xs = zip (pad $ freqDist xs) (freqDist xs)

pad :: [String] -> [String]
pad columns = map (concat . (\i -> replicate (abs $ len columns - length i) " ")) columns

addNewlines :: [String] -> [String]
addNewlines = intersperse "\n"

intersperseString :: forall b. [b] -> [b] -> [b]
intersperseString = ((=<<) . flip (:))

len :: [String] -> Int
len = fst . maxFst . zipLengths

maxFst :: [(Int, String)] -> (Int, String) 
maxFst = maximumBy (\a b -> if fst a > fst b then GT else LT)

zipLengths :: [String] -> [(Int, String)]
zipLengths xs = zip (map length xs) xs

freqDist :: [Integer] -> [String]
freqDist = map tower . freq

freq :: forall k a. (Ord k, Num a) => [k] -> [(k, a)]
freq input = toList $ fromListWith (+) [(c, 1) | c <- input]

-- IDEA: don't build the tower here, instead do it as the last step and use
-- `Data.List.intercalate`
tower :: (a, Integer) -> String
tower (_, frq) = concat ["*" | _ <- [1..frq]]

main :: IO ()
main = undefined
