{-# LANGUAGE UnicodeSyntax #-}

module AParser (Parser, runParser, satisfy, char, posInt) where

import           Control.Applicative
import           Data.Char
import           Control.Monad

newtype Parser a = Parser { runParser ∷ String → Maybe (a, String) }

satisfy ∷ (Char → Bool) → Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing

char ∷ Char → Parser Char
char c = satisfy (== c)

posInt ∷ Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

inParser ∷ ((String → Maybe (a1, String)) → String → Maybe (a, String)) → Parser a1 → Parser a
inParser f = Parser . f . runParser

first ∷ (a → b) → (a,c) → (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
  fmap = inParser . fmap . fmap . first

instance Applicative Parser where
  pure a = Parser (\s → Just (a, s))
  (Parser fp) <*> xp = Parser $ \s →
    case fp s of
      Nothing     → Nothing
      Just (f,s') → runParser (f <$> xp) s'

instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2

abParser ∷ Parser (Char, Char)
abParser = (,) <$> (char 'a') <*> (char 'b')

abParser_ ∷ Parser ()
abParser_ = (\_ → ()) <$> abParser

intPair ∷ Parser [Integer]
intPair = (\a _ b → [a,b]) <$> posInt <*> (char ' ') <*> posInt

oneInt ∷ Parser ()
oneInt = (\_ → ()) <$> posInt

upper ∷ Parser ()
upper = (\_ → ()) <$> satisfy isUpper

intOrUppercase ∷ Parser ()
intOrUppercase = upper <|> oneInt

data Foo = Bar Integer Integer Char

parseFoo :: Parser Foo
parseFoo = Bar <$> posInt <*> posInt <*> satisfy isAlpha

{-

`Applicative` gives us no way to decide what to do next based on previous 
results --- we must decide in advance what parsing operations we are going 
to run, before we see the results

-}

-- (>>=) :: m a -> (a -> m b) -> m b
{-
 
 `(>>=)` takes two arguments. The first one is of type `m a`, sometimes called computation `m a`. 
 It is a computation that results in a value of type `a` and may also have some sort of "effect".

* `c1 :: Maybe a` is a computation which might fail but results in an `a` if it succeeds.

* `c2 :: [a]` is a computation which results in multiple `a`s.

* `c3 :: Parser a` is a computation which implicitly consumes part of a String and (possibly) produces an `a`.

* `c4 :: IO a` is a computation which potentially has some I/O effects and then produces an `a`.

And so on. What about the second argument to `(>>=)`? It is a *function* of type `(a -> m b)`. That is, it is a function which will *choose* the next computation to run based on the result(s) of the first computation. This is precisely what embodies the promised power of `Monad` to encapsulate computations which can choose what to do next based on the results of previous compuations.

So all `(>>=)` really does is put together two computations to produce a larger one, which first runs one and then the other, returning the results of the second one. The all--important twist is that we get to decide *which* computation to run second based on the output from the first.

The default implementation of `(>>)` should make sense now:

    (>>) :: m a -> m b -> m b
    m1 >> m2 = m1 >>= \_ -> m2

`m1 >> m2` simply does `m1` then `m2`, ignoring the result of `m1`.

-}

check :: Int -> Maybe Int
check n | n < 10    = Just n
        | otherwise = Nothing

halve :: Int -> Maybe Int
halve n | even n    = Just $ n `div` 2
        | otherwise = Nothing

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1, x+2]
