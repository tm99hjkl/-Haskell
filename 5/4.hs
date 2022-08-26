perfects :: Int -> [Int]
perfects n = [p | p <- [2 .. n], sum (factors p) - p == p]

factors :: Int -> [Int]
factors n = [m | m <- [1 .. n], n `mod` m == 0]