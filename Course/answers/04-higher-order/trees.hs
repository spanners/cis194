{-# OPTIONS_GHC -Wall #-}

module Tree where

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)


