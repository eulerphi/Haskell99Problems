-- 0 based index
-- Note to self: using x xs is too similar to x:xs
-- use x ys instead
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs index
  | index < 0 = error "index < 0"
  | index > (length xs) = error "index > (length xs)"
insertAt x xs index = (take index xs) ++ (x : drop index xs)

-- take and drop are just so nice
-- but this online solution shows
-- how nice recursion works for
-- this problem
insertAt' :: a -> [a] -> Int -> [a]
insertAt' x ys     1 = x:ys
insertAt' x (y:ys) n = y:insertAt' x ys (n-1)
