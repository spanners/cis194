{-# OPTIONS_GHC -Wall #-}

-- look at 
-- https://stackoverflow.com/questions/16246456/sieve-of-sundaram-list-comprehension

module SieveSundaram where

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\x -> 2 * x + 1) . filter f . enumFromTo 1 
    where
      f x = all (\(i,j) -> i + j + 2 * i * j > x) [(i,j) | i <- [1..x], j <- [1..x], i <= j]
