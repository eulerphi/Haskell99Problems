slice :: [a] -> Int -> Int -> [a]
slice xs start end = (take toTake) . (drop toDrop) $ xs
  where toDrop = start - 1
        toTake = end - start + 1

-- Online solution
slice' xs i k | i>0 = take (k-i+1) $ drop (i-1) xs

-- Guards are a good idea
slice'' :: [a] -> Int -> Int -> [a]
slice'' xs start end | start>0 && end>=start = (take toTake) . (drop toDrop) $ xs
  where toDrop = start - 1
        toTake = end - start + 1

-- Derp.. taking first is cleaner.. although I wonder
-- if another list is created under the hood.
-- Dropping first likely gives you a pointer into the
-- existing list
slice''' xs i k = drop (i-1) $ take k xs
