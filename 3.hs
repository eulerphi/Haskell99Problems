elementAt :: [a] -> Int -> Maybe a
elementAt _ n | n < 1 = Nothing
elementAt [] n | n > 1 = Nothing
elementAt (x:xs) n | n == 1 = Just x
elementAt(x:xs) n = elementAt xs (n - 1)
