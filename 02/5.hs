myInit :: [a] -> [a]
myInit l = take (length l - 1) l

myInit2 :: [a] -> [a]
myInit2 l = reverse $ tail $ reverse l