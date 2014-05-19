import System.Random

-- Can't say that I completely understand what is
-- going on here... does g' vs g'' make a difference?
rnd_select :: RandomGen g => [a] -> Int -> g -> ([a], g)
rnd_select _  0 g = ([], g)
rnd_select [] _ g = ([], g)
rnd_select xs n g = (r:rs, g'')
  where
    (index, g') = randomR (0, (length xs) - 1) g
    (r, xs') = removeAt index xs
    (rs, g'') = rnd_select xs' (n - 1) g'

rnd_selectIO :: [a] -> Int -> IO [a]
rnd_selectIO l count = getStdRandom $ rnd_select l count

removeAt 0 (x:xs) = (x, xs)
removeAt n (x:xs) = (elem, x:xs')
  where (elem, xs') = removeAt (n - 1) xs

