module Employee where

import AParser (Parser, char, runParser, posInt)
import Control.Applicative

type Name = String
data Employee = Emp { name :: Name, phone :: String }

parseName :: Parser Name
parseName = undefined

parsePhone :: Parser String
parsePhone = undefined

foo :: Parser Employee
foo = Emp <$> parseName <*> parsePhone

abParser :: Parser (Char, Char)
abParser = (,) <$> (char 'a') <*> (char 'b')

abParser_ :: Parser ()
abParser_ = (\_ -> ()) <$> abParser

intPair :: Parser [Integer]
intPair = (\a _ b -> [a,b]) <$> posInt <*> (char ' ') <*> posInt

-- runParser abParser "abcdef"
-- Just ((’a’,’b’),"cdef")
