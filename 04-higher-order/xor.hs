{-# OPTIONS_GHC -Wall #-}

module Xor where

import Data.List (foldl')

xor :: [Bool] -> Bool
xor = foldl' xorF False
  where 
    xorF :: Bool -> Bool -> Bool
    xorF True  False  = True
    xorF True  True   = False
    xorF False True   = True
    xorF False False  = False
