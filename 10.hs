import Data.List as List

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode list@(x:xs) = encodeGroups . List.group $ list
  where
    encodeGroups [] = []
    encodeGroups (y:ys) = (length y, head y) : encodeGroups ys

-- Point free
encode' :: Eq a => [a] -> [(Int, a)]
encode' = map (\x -> (length x, head x)) . group
