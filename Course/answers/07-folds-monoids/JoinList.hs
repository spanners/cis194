{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Data.Monoid
import Sized
import Test.QuickCheck
import Control.Monad

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

jTree = sized jTree'
jTree' 0 = liftM3 Single arbitrary arbitrary
jTree' n | n>0 = 
    oneof [liftM2 Single arbitrary arbitrary,
           liftM3 Append arbitrary subtree subtree]
             where subtree = jTree' (n `div` 2)

instance Arbitrary (JoinList (Monoid m) (Sized a)) where
  arbitrary = sized jTree'
    where jTree' 0 = liftM2 Single arbitrary arbitrary
          jTree' n | n>0 = 
              oneof [liftM2 Single arbitrary arbitrary,
                    liftM3 Append arbitrary subtree subtree]
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

    
