duplicateElements :: [a] -> [a]
duplicateElements [] = []
duplicateElements (x:xs) = x : x : duplicateElements xs
