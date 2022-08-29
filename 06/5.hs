-- merge
-- merge [2,5,6] [1,3,4]  -> [1,2,3,4,5,6]
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x <= y     = x : merge xs (y : ys)
  | otherwise  = y : merge (x : xs) ys

-- halve
halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (take half xs, drop half xs)
    where
        half = length xs `div` 2


-- msort
-- msort [x] を定義しないと、[x]を分割し続けて詰む
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort $ fst (halve xs))  (msort $ snd (halve xs))

