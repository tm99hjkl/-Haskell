-- myany
myany :: (a -> Bool) -> [a] -> Bool
myany p xs = not . null $ filter p xs

test1 :: Bool
test1 = myany odd [2, 4, 6, 8] == False

test2 :: Bool
test2 = myany odd [1, 2, 3, 4] == True

-- myall
myall :: (a -> Bool) -> [a] -> Bool
myall p = not . myany p

test3 :: Bool
test3 = myall odd [2, 4, 6, 8] == False

test4 :: Bool
test4 = myall odd [1, 3, 5, 7] == True

-- takeWhile
mytakeWhile :: (a -> Bool) -> [a] -> [a]
mytakeWhile p [] = []
mytakeWhile p (x : xs)
  | p x = acc ++ [x] ++ mytakeWhile p xs
  | otherwise = acc
  where
    acc = []

-- dropWhile
mydropWhile :: (a -> Bool) -> [a] -> [a]
mydropWhile p [] = []
mydropWhile p (x : xs)
  | p x = mydropWhile p xs
  | otherwise = x : xs