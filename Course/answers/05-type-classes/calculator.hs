{-# OPTIONS_GHC -Wall #-}

module Calculator where

import ExprT(ExprT(..))
import Parser(parseExp)
import Control.Applicative

eval :: ExprT -> Integer
eval (Lit n)   = n
eval (Mul x y) = eval x * eval y
eval (Add x y) = eval x + eval y

evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp Lit Add Mul s
