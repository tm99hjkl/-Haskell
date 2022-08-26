scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

sptest1 = scalarproduct [1,2,3] [1,2,3] == 14