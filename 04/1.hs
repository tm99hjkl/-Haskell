halve :: [a] -> ([a], [a])
halve l = splitAt half l
    where
        half = length l `div` 2