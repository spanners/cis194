{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module StringBuffer where

import Data.Monoid

import Buffer

instance Buffer String where
  toString     = id
  fromString   = id
  line n b     = (lines b) !? n 
  replaceLine n l b 
               = unlines . uncurry replaceLine' 
                         . splitAt n 
                         . lines $ b
      where replaceLine' pre [] = pre
            replaceLine' pre (_:ls) = pre ++ l:ls
  numLines     = length . lines
  value        = length . words

(!?) :: Int -> [a] -> Maybe a
_      !? n | n < 0 = Nothing
[]     !? _         = Nothing
(x:_)  !? 0         = Just x
(_:xs) !? n         = safeIndex (n-1) xs
