import Data.List as List

data EncodedItem a = Single a | Multiple (Int, a) deriving Show

-- Original solution
decode :: Eq a => [EncodedItem a] -> [a]
decode [] = []
decode (x:xs) = (decodeItem x) ++ (decode xs)
  where
    decodeItem (Single y) = [y]
    decodeItem (Multiple (2, y)) = [y, y]
    decodeItem (Multiple (n, y)) = y :  decodeItem (Multiple (n - 1, y))

-- Online solution
decode' :: [EncodedItem a] -> [a]
decode' = concatMap decodeHelper
    where
      decodeHelper (Single x) = [x]
      decodeHelper (Multiple (n x)) = replicate n x
