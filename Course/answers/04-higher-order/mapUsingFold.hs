{-# OPTIONS_GHC -Wall #-}

module MapAsFold where

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f list = foldr (\y ys -> (f y):ys) [] list 
