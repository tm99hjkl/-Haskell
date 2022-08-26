find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k_, v) <- t, k_ == k ]

-- position: 要素がリストの何番目にあるか　のリストを返す
-- position 'a' "123abc" --> [4]
position :: Eq a => a -> [a] -> [Int]
position x xs = find x (zip xs [1..length xs])