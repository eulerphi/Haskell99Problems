import Data.List as List

-- Original solution
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) | x == y = compress (x:xs)
compress (x:y:xs) | x /= y = x : compress (y:xs)

-- Cleaned up version of above
compress' :: (Eq a) => [a] -> [a]
compress' (x:xs@(y:_))
	| x == y = compress' xs
	| otherwise = x : compress' xs
compress' x = x

-- Functional version...
compress'' :: Eq a => [a] -> [a]
compress'' = map head . List.group
