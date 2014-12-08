{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module JoinList where

import Data.Monoid
import Control.Applicative
import Control.Monad
import Sized
import StringBuffer ((!?))
import Test.QuickCheck
import Test.QuickCheck.All

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

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
indexJ _ Empty             = Nothing
indexJ i _  | i < 0        = Nothing
indexJ i jl | i > sizeJ jl = Nothing
indexJ _ (Single _ a)      = Just a
indexJ i (Append _ l r) 
    | i < sizeJ l          = indexJ i l
    | otherwise            = indexJ (i - sizeJ l) r

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

instance Arbitrary (JoinList (Sum Int) Char) where
    arbitrary = sized joinList'
      where joinList' 0       = pure Empty
            joinList' n | n>0 = 
                oneof [ liftM2 Single (pure (Sum n)) arbitrary
                      , liftM3 Append (pure (Sum n)) subList subList
                      ] 
                  where subList = joinList' (n `div` 2)

-- sample (arbitrary :: Gen (JoinList (Sum Int) Char))

--prop_indexJ :: (Arbitrary m, Sized m, Monoid m, Arbitrary a, Eq a) => Int -> JoinList m a -> Bool
--prop_indexJ i jl = (indexJ i jl) == (jlToList jl !? i)

runTests :: IO Bool
runTests = $(quickCheckAll)

main :: IO Bool
main = runTests
