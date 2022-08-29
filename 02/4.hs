myLast1 :: [a] -> a
myLast1 lst = head $ reverse lst

myLast2 :: [a] -> a
myLast2 lst = lst !! (length lst - 1)
