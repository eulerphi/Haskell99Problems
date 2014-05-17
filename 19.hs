rotate :: [a] -> Int -> [a]
rotate xs n | n < 0 = rotate xs (length xs + n)
rotate xs n | n > (length xs) = rotate xs (n `mod` (length xs))
rotate xs n | n >= 0 = (drop n xs) ++ (take n xs)

-- Very cool online solution :)
rotate' xs n = take (length xs) $ drop (length xs + n) $ cycle xs
