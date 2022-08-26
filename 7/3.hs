-- map
mymap :: (a -> b) -> [a] -> [b]
mymap f = foldr (\x xs -> f x : xs) []


-- filter
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter p = foldr (\y ys -> if p y then y : ys else ys) []