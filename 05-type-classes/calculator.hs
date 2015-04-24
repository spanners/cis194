{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, UnicodeSyntax #-}

module Calculator where

import ExprT as E
import Parser (parseExp)
import StackVM as S

eval ∷ ExprT → Integer
eval (E.Lit n)   = n
eval (E.Mul x y) = eval x * eval y
eval (E.Add x y) = eval x + eval y

evalStr ∷ String → Maybe Integer
evalStr = fmap eval . parseExp E.Lit E.Add E.Mul

class Expr a where
    lit ∷ Integer → a
    mul ∷ a → a → a
    add ∷ a → a → a

instance Expr ExprT where
    lit = E.Lit
    add = E.Add
    mul = E.Mul

reify ∷ ExprT → ExprT
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


testExp ∷ Expr a ⇒ Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger ∷ Maybe Integer
testInteger = testExp

testBool ∷ Maybe Bool
testBool = testExp

testMM ∷ Maybe MinMax
testMM = testExp 

testSat ∷ Maybe Mod7
testSat = testExp

instance Expr Program where
    lit a   = [S.PushI a]
    add a b = a ++ b ++ [S.Mul]
    mul a b = a ++ b ++ [S.Add]

compile ∷ String → Maybe S.Program
compile = parseExp lit add mul

evalCode ∷ String → Maybe (Either String S.StackVal)
evalCode = fmap stackVM . parseExp lit add mul

exprTFold ∷ (Integer → t) → (t → t → t) → (t → t → t) → ExprT → t
exprTFold f _ _ (E.Lit i)     = f i
exprTFold f g h (E.Add e1 e2) = g (exprTFold f g h e1) (exprTFold f g h e2)
exprTFold f g h (E.Mul e1 e2) = h (exprTFold f g h e1) (exprTFold f g h e2)

eval2 ∷ ExprT → Integer
eval2 = exprTFold id (+) (*)

numLiterals ∷ ExprT → Integer
numLiterals = exprTFold (const 1) (+) (+)
