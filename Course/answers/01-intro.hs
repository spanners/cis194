toDigits :: Integer -> [Integer] 
toDigits n | n <= 0 = []
           | otherwise = [ read [ch] | ch <- show n ]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) $ cycle [1,2]

main :: IO ()
main = undefined
