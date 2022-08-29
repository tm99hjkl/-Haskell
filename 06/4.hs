-- merge
-- merge [2,5,6] [1,3,4]  -> [1,2,3,4,5,6]
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x <= y     = x : merge xs (y : ys)
  | otherwise  = y : merge (x : xs) ys
