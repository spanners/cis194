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
