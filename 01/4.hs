qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) =
    qsort large ++ [x] ++ qsort small 
    where
        small = [a | a <- xs, a <= x]
        large = [b | b <- xs, b > x]