range :: (Enum a, Ord a) => a -> a -> [a]
range start end
  | start > end = []
  | otherwise   = start : range (succ start) end
