-- sum 
mysum :: Num a => [a] -> a
mysum [] = 0
mysum (x : xs) = x + mysum xs


-- take : リストの先頭からnこの要素を取り出す
mytake :: Int -> [a] -> [a]
mytake 0 _ = []
mytake _ [] = []
mytake n (x:xs) = x : mytake (n - 1) xs


-- last : 空でないリストの末尾の要素を取り出す関数
mylast :: [a] -> a
mylast [x] = x
mylast (_ : xs) = mylast xs