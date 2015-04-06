--{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module JoinList where

import Data.Monoid
import Sized
import Test.QuickCheck
import Test.QuickCheck.All
import Control.Monad

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

instance (Arbitrary a, Sized a) => Arbitrary (JoinList Size a) where
  arbitrary = sized jTree'

jTree' :: (Arbitrary a1, Arbitrary a2) => Int -> Gen (JoinList a1 a2)
jTree' 0 = liftM2 Single arbitrary arbitrary
jTree' n | n>0 =
    oneof [ liftM2 Single arbitrary arbitrary
          , liftM3 Append arbitrary subtree subtree ]
      where subtree = jTree' (n `div` 2)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 `mappend` tag jl2) jl1 jl2

example :: JoinList (Product Integer) Char
example = Append (Product 210)
           (Append (Product 30)
             (Single (Product 5) 'y')
             (Append (Product 6)
               (Single (Product 2) 'e')
               (Single (Product 3) 'a')))
           (Single (Product 7) 'h')

simple :: JoinList (Sum Integer) Char
simple = Append (Sum 5) (Single (Sum 2) 'a') (Single (Sum 3) 'b')

sizeJ :: (Sized b, Monoid b) => JoinList b a -> Int
sizeJ = getSize . size . tag

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty  = Nothing
indexJ i (Single m d)
    | i == 0    = Just d
    | otherwise = Nothing
indexJ i (Append m jL1 jL2)
    | i < 0     = Nothing
    | i >= getSize (size m) = Nothing     -- optional, more efficient to have it
    | i < s1    = indexJ i jL1
    | otherwise = indexJ (i - s1) jL2
      where
        s1 = getSize . size $ tag jL1

(!!?) :: Int -> [a] -> Maybe a
_ !!? []        = Nothing
i !!? _ | i < 0 = Nothing
0 !!? (x:xs)    = Just x
i !!? (x:xs)    = (i-1) !!? xs

prop_indexJ :: Int -> (JoinList Size Size) -> Bool
prop_indexJ i jl = (indexJ i jl) == (i !!? jlToList jl)

{- This fails with the following inputs
 - 0
 - Append (Size 1) (Single (Size 0) (Size 0)) (Single (Size (-1)) (Size 1)) -}

{- WHY? 
 - 
 - 
 -
 - -}

runTests :: IO Bool
runTests = $(quickCheckAll)

main :: IO ()
main = quickCheck prop_indexJ
