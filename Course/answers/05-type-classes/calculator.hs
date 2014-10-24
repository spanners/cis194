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
    lit = (>0)
    add = (||)
    mul = (&&)
