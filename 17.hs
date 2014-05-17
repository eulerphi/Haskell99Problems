-- Depending on your interpretation of the question...
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

-- Let us make this harder...
split' :: [a] -> Int -> ([a], [a])
split' xs n = (fst' xs n, snd' xs n)
  where
    fst' [] _ = []
    fst' _  0 = []
    fst' (x:xs) n = [x] ++ (fst' xs (n - 1))
    snd' [] _ = []
    snd' xs 0 = xs
    snd' (x:xs) n = snd' xs (n - 1)
