skips :: [a] -> [[a]]
skips xs = helper (length xs) xs

helper :: Int -> [a] -> [[a]]
helper 0 _ = []
helper n xs = fst (splitAt n xs):helper (n-1) xs
