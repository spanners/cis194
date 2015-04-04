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

sizeJ :: (Sized b, Monoid b) => JoinList b a -> Int
sizeJ = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty        = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ n node@(Append _ jll jlr)
  | n >= sizeJ node = Nothing
  | n >= sizeJ jll  = indexJ (n - sizeJ jll) jlr
  | otherwise       = indexJ n jll

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(!?) = undefined

--prop_indexJ :: (Monoid m, Sized m, Eq a) => Int -> JoinList m a -> Bool
--prop_indexJ i jl = (indexJ i jl) == (jlToList jl !? i)

runTests :: IO Bool
runTests = $(quickCheckAll)

-- main :: IO Bool
-- main = quickCheck prop_indexJ
