{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calculator where

import ExprT(ExprT(..))
import Parser(parseExp)

eval :: ExprT -> Integer
eval (Lit n)   = n
eval (Mul x y) = eval x * eval y
eval (Add x y) = eval x + eval y

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (<0)
    add = (||)
    mul = (&&)

newtype Mod7 = Mod7 Integer deriving (Eq, Show, Enum, Ord, Real, Integral, Num)
newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

instance Expr MinMax where
    lit = MinMax
    add = min
    mul = max

instance Expr Mod7 where
    lit = Mod7
    add i j = (i + j) `mod` 7
    mul i j = (i * j) `mod` 7


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp

testBool :: Maybe Bool
testBool = testExp

testMM :: Maybe MinMax
testMM = testExp 

testSat :: Maybe Mod7
testSat = testExp
