removeAt :: Int -> [a] -> (a, [a])
removeAt i _  | i < 0 = error "i < 0"
removeAt i xs | i >= (length xs) = error "i >= length xs"
removeAt i xs = (el, ys)
  where el = xs !! i
        ys = (take i xs) ++ (drop (i + 1) xs)

-- interesting online solution
removeAt' 1 (x:xs) = (x, xs)
removeAt' n (x:xs) = (l, x:r)
  where (l, r) = removeAt' (n - 1) xs
