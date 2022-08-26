safetailA :: [a] -> [a]
safetailA l = 
    if null l then []
    else drop 1 l

safetailB :: [a] -> [a]
safetailB l 
    | null l = []
    | otherwise = drop 1 l


safetailC :: [a] -> [a]
safetailC [] = []
safetailC (x:xs) = xs
