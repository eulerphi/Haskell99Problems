import Data.List as List

-- I remember List.group doing exactly
-- what is needed from problem 8...
pack :: Eq a => [a] -> [[a]]
pack = List.group

-- That was too easy, let's make a mess
-- of this...
--pack' :: Eq a => [a] -> [[a]] -> [[a]]
--pack' (x:xs) [] = pack' xs [[x]]
--pack' (x:xs) acc@(f@(y:ys):zs)
-- | x ==  y = pack' xs (([x] ++ f):zs)
-- | x /=  y = pack' xs ([x]:acc)
--pack' [x] [] = [[x]]
--pack' [x] acc@(f@(y:ys):zs)
-- | x ==  y = (([x] ++ f):zs)
-- | x /=  y = ([x]:acc)

-- Solution online...
-- pack' :: Eq a => [a] -> [[a]]
-- pack' [] = []
-- pack' (x:xs) = (x:first) : pack' rest
         -- where
           -- getReps [] = ([], [])
           -- getReps (y:ys)
                   -- | y == x = let (f,r) = getReps ys in (y:f, r)
                   -- | otherwise = ([], (y:ys))
           -- (first,rest) = getReps xs

-- let's see if I actually understand this
-- online solution
pack' :: Eq a => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = (x:xList) : pack'' nextList
  where
    (xList, nextList) = split xs
    split [] = ([], [])
    split (y:ys)
      | x == y = (y:yList, nextList)
      | otherwise = ([], (y:ys))
        where (yList, nextList) = split ys

-- let's look at a more (?) functional online 
-- solution..
-- pack (x:xs) = let (first,rest) = span (==x) xs
               -- in (x:first) : pack rest
-- pack [] = []

-- Let's try rewriting this as well...
pack'' :: Eq a => [a] -> [[a]]
pack'' [] = []
pack'' (x:xs) = (x:xs') : pack'' xs''
  where (xs', xs'') = span (== x) xs
