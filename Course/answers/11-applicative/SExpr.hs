{- CIS 194 HW 11
   due Monday, 8 April
-}

{-# LANGUAGE UnicodeSyntax #-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- Holy mackerel! These are two beautiful, mutually recursive definitions ♥

zeroOrMore ∷ Parser a → Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore ∷ Parser a → Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

-- If Haskell had idiom brackets, oneOrMore could look like this:
-- oneOrMore p = ⟦ p : zeroOrMore p ⟧

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces ∷ Parser String
spaces = (:) <$> satisfy isSpace <*> spaces

ident ∷ Parser String
ident = (:) <$> satisfy isAlpha <*> alphaNums

alphaNums :: Parser String
alphaNums = (:) <$> satisfy isAlphaNum <*> ident
