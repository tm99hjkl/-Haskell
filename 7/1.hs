-- rewrite [f x | x <- xs, p x]
filtermap :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtermap f p xs = map f $ filter p xs
