replicateElements :: [a] -> Int -> [a]
replicateElements [] _ = []
replicateElements (x:xs) n = (replicate n x) ++ (replicateElements xs n)

-- concatMap, yes.. yes..
replicateElements' :: [a] -> Int -> [a]
replicateElements' xs n = concatMap (replicate n) xs
