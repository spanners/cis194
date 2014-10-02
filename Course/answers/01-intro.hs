{-# OPTIONS_GHC -Wall #-}

-- Credit card validator

toDigits :: Integer -> [Integer] 
toDigits n | n <= 0 = []
           | otherwise = [ read [ch] | ch <- show n ]

toStrings :: [Integer] -> [String]
toStrings ns = [ show ch | ch <- ns ] 

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) (cycle [1,2])

toInteger_ :: String -> Integer
toInteger_ s = read s

sumDigits :: [Integer] -> Integer
sumDigits = sum . toDigits . toInteger_ . concat . toStrings

validate :: Integer -> Bool
validate = (==0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

eachLine :: (String -> String) -> (String -> String)
eachLine f = unlines . map f . lines

validator :: String -> String
validator = show . validate . read

main :: IO ()
main = interact (eachLine validator)
