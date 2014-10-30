{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Fibonacci where

fib :: Integer -> Integer
fib = round . (/ sq5) . (phi **) . fromIntegral
  where sq5 = sqrt 5 :: Double
        phi = (1 + sq5) / 2

fib1 :: [Integer]
fib1 = map fib [0..]

data Stream a = SCons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (SCons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = SCons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (SCons x xs) = SCons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = SCons (f x) (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+1) (-1)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (SCons x s1) s2 = SCons x (interleaveStreams s2 s1)

ruler :: Stream Integer
ruler = foldr1 interleaveStreams (map streamRepeat [0..])

x' :: Stream Integer
x' = SCons 0 (SCons 1 (streamRepeat 0))

streamZipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamZipWith f (SCons x xs) (SCons y ys) = SCons (f x y) (streamZipWith f xs ys)

instance Num (Stream Integer) where
    fromInteger n                    = SCons n (streamRepeat 0)
    negate                           = streamMap negate
    (+)                              = streamZipWith (+)
    (*) (SCons x xs) s2@(SCons y ys) = SCons (x * y) ((streamMap (* x) ys) + (xs * s2))

