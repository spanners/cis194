{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes #-}

module Histogram where

import Data.Map (toList, fromListWith)
import Data.List (maximumBy)


skim :: [String] -> [[String]]
skim foo = map (\i -> replicate (abs $ len foo - length i) " \n") foo
-- GHCi> zip (map (\i -> concat $ replicate (abs $ len bar - length i) " \n") bar) bar
-- > [("","aaa"),(" \n","bb"),(" \n \n","c")]

len :: [String] -> Int
len = fst . maxFst . zipLengths

maxFst :: [(Int, String)] -> (Int, String) 
maxFst = maximumBy (\a b -> if fst a > fst b then GT else LT)

zipLengths :: [String] -> [(Int, String)]
zipLengths xs = zip (map length xs) xs

histogram :: [Integer] -> [String]
histogram = map tower . freq

freq :: forall k a. (Ord k, Num a) => [k] -> [(k, a)]
freq input = toList $ fromListWith (+) [(c, 1) | c <- input]

-- IDEA: don't build the tower here, instead do it as the last step and use
-- `Data.List.intercalate`
tower :: (a, Integer) -> String
tower (_, frq) = concat ["*\n" | _ <- [0..frq]]

main :: IO ()
main = undefined
