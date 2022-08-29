pow :: Num a => a -> Int -> a
_ `pow` 0 = 1
x `pow` n = x * (x `pow` (n-1))