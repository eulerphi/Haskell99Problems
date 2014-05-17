-- what you want is drop :)
skip :: Int -> [a] -> [a]
skip _ [] = []
skip 0 xs = xs
skip n xs = skip (n - 1) (tail xs)

drop' :: [a] -> Int -> [a]
drop' [] _ = []
drop' xs n = (take (n - 1) xs) ++ (drop' (skip n xs) n)
