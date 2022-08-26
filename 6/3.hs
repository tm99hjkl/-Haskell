myAnd :: [Bool] -> Bool 
myAnd [] = True 
myAnd (x : xs)
  | x = myAnd xs
  | otherwise = False


-- concat
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x : xs) = x ++ myConcat xs


-- replicate
myRep :: Int -> a -> [a]
myRep 0 _ = []
myRep n x = x : myRep (n - 1) x


-- !! (index access)
indx :: [a] -> Int -> a
indx (x : _) 0 = x
indx (_ : xs) n = indx xs (n - 1)


-- elem
myElem :: Eq a => a -> [a] -> Bool 
myElem _ [] = False 
myElem x (y:ys) 
  | x == y  = True 
  | otherwise = myElem x ys