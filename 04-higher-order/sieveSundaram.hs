{-# OPTIONS_GHC -Wall #-}

module SieveSundaram where

cartProd :: [a] -> [(a, a)]
cartProd xs = [(x,y) | x <- xs, y <- xs]

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map ((+1) . (*2)) . filter valid 
                                  . enumFromTo 1

valid :: Integer -> Bool
valid x = not . any (\(i,j) -> i + j + 2*i*j == x) 
              . filter (uncurry (<=)) $ cartProd [1..x]
