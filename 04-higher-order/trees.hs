{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UnicodeSyntax #-}

module Tree where

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)


foldTree :: [a] -> Tree a
foldTree = foldr (\x tree -> insertTree x tree) Leaf

singleton :: a -> Tree a
singleton x = Node 0 Leaf x Leaf

heightTree :: Tree a -> Integer
heightTree Leaf = 0
heightTree (Node n _ _ _) = n

insertTree :: a -> Tree a -> Tree a
insertTree x Leaf = singleton x
insertTree x (Node n left val right) 
    | h1 < h2 =   Node n     (insertTree x left) val right
    | h1 > h2 =   Node n     left                val rightN
    | otherwise = Node (h+1) left                val rightN
  where h1     = heightTree left
        h2     = heightTree right
        rightN = insertTree x right
        h      = heightTree rightN
