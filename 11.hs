import Data.List as List

data EncodedItem a = Single a | Multiple (Int, a) deriving Show

encode :: Eq a => [a] -> [EncodedItem a] 
encode = map toEncodedItem . List.group
  where
    toEncodedItem [y] = Single y
    toEncodedItem ys =  Multiple (length ys, head ys)
