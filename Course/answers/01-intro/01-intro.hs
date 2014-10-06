{-# OPTIONS_GHC -Wall #-}

import Data.Char

-- Credit card validator


intToString :: Int -> String
intToString n = (if d /= 0 then intToString d else "") ++ [intToDigit m]
                  where (d, m) = n `divMod` 10 

toDigits :: Integer -> [Integer]
toDigits = map (toInteger . digitToInt) . intToString . fromIntegral

toDigitsRev :: Integer -> [Integer] 
toDigitsRev = reverse . toDigits

toStrings :: [Integer] -> [String]
toStrings = map show 

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) (cycle [1,2])

toInteger_ :: String -> Integer
toInteger_ = read

sumDigits :: [Integer] -> Integer
sumDigits = sum . toDigits . toInteger_ . concat . toStrings

validate :: Integer -> Bool
validate = (==0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigitsRev

eachLine :: (String -> String) -> (String -> String)
eachLine f = unlines . map f . lines

validator :: String -> String
validator = show . validate . read

main :: IO ()
main = interact (eachLine validator)
