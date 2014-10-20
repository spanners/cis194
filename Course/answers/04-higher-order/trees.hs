{-# OPTIONS_GHC -Wall #-}
{-# UnicodeSyntax #-}

module Tree where

import Data.Foldable as F
import Data.Monoid

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)


foldTree :: [a] -> Tree a
foldTree = undefined

instance F.Foldable Tree where
    foldMap f Leaf = mempty
    foldMap f (Node i l e r) = undefined
