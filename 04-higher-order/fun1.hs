{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

module Fun1 where

import Data.List (foldl')

fun1 :: [Integer] -> Integer
fun1 = foldl' f 1
  where
    f :: Integer -> Integer -> Integer
    f x = if even x then ((x - 2)*) else id

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (>1) . iterate collatz 
  where
    collatz :: Integer -> Integer
    collatz n = if even n then n `div` 2 else 3 * n + 1
