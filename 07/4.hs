-- dec2int
-- dec2int [2,3,4,5] = 2345
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> x * 10 + y) 0

test1 :: Bool
test1 = dec2int [2,3,4,5] == 2345