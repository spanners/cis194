{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall #-}

module Fibonacci where

fib ∷ Integer → Integer
fib = round . (/ sq5) . (phi **) . fromIntegral
  where sq5 = sqrt 5 ∷ Double
        phi = (1 + sq5) / 2

fibs1 ∷ [Integer]
fibs1 = map fib [0..]

data Stream a = SCons a (Stream a)

streamToList ∷ Stream a → [a]
streamToList (SCons x xs) = x : streamToList xs

instance Show a ⇒ Show (Stream a) where
    show = show . take 20 . streamToList

streamRepeat ∷ a → Stream a
streamRepeat x = SCons x (streamRepeat x)

streamMap ∷ (a → b) → Stream a → Stream b
streamMap f (SCons x xs) = SCons (f x) (streamMap f xs)

streamFromSeed ∷ (a → a) → a → Stream a
streamFromSeed f x = SCons (f x) (streamFromSeed f (f x))

nats ∷ Stream Integer
nats = streamFromSeed (+1) (-1)

interleaveStreams ∷ Stream a → Stream a → Stream a
interleaveStreams (SCons x s1) s2 = SCons x (interleaveStreams s2 s1)

ruler ∷ Stream Integer
ruler = foldr1 interleaveStreams (map streamRepeat [0..])

x' ∷ Stream Integer
x' = SCons 0 (SCons 1 (streamRepeat 0))

streamZipWith ∷ (a → b → c) → Stream a → Stream b → Stream c
streamZipWith f (SCons x xs) (SCons y ys) = SCons (f x y) (streamZipWith f xs ys)

instance Num (Stream Integer) where
    fromInteger n = SCons n (streamRepeat 0)
    negate        = streamMap negate
    (+)           = streamZipWith (+)
    (*) (SCons x xs) s2@(SCons y ys) 
                  = SCons (x * y) (streamMap (* x) ys + (xs * s2))

instance Fractional (Stream Integer) where
    (/) (SCons a₀ a') (SCons b₀ b') = q
      where q = SCons (a₀ `div` b₀) (streamMap (`div` b₀) (a' - q*b'))

fibs2 ∷ Stream Integer
fibs2 = x' / (1 - x' - x'*x')

data Matrix = Matrix Integer Integer Integer Integer deriving Show

instance Num Matrix where
    (*) (Matrix a₁₁ a₁₂ a₂₁ a₂₂) (Matrix b₁₁ b₁₂ b₂₁ b₂₂) = (Matrix (a₁₁*b₁₁ + a₁₂*b₂₁) (a₁₁*b₁₂ + a₁₂*b₂₂) (a₂₁*b₁₁ + a₂₂*b₂₁) (a₂₁*b₁₂ + a₂₂*b₂₂))

fib2 ∷ Integer → Integer
fib2 0 = 0
fib2 n = project $ (Matrix 1 1 1 0)^n
           where project (Matrix _ fn _ _) = fn
