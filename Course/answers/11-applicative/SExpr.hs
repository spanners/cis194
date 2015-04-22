{- CIS 194 HW 11
   due Monday, 8 April
-}

{-# LANGUAGE UnicodeSyntax #-}

module SExpr where

import AParser
import Control.Applicative

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
spaces = undefined

ident ∷ Parser String
ident = undefined

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show
