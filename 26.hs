combinations :: Int -> [a] -> [[a]]

-- original solution
combinations n xs = helper n xs []
  where
    helper 0 _  rs = [rs]
    helper _ [] _  = []
    helper n (x:xs) rs = (helper (n-1) xs (rs ++ [x])) ++ (helper n xs rs)

-- online solution: map (x:) very slick
combinations' :: Int -> [a] -> [[a]]
combinations' 0 _      = [[]]
combinations' _ []     = []
combinations' n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs) 
