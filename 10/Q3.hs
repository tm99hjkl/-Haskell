data Tree = Leaf Int | Node Tree Tree
  deriving (Show)
t :: Tree
t = Node (Node (Node (Leaf 3) (Node (Leaf 2) (Leaf 1))) (Node (Leaf 1) (Leaf 2))) (Node (Leaf 1) (Leaf 2))

t2 :: Tree
t2 = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 1) (Leaf 2))
-- balance : 平衡木かどうか判定する関数
--           平衡木とは，左右の部分木の葉の数が高々1つ違う木
balanced :: Tree -> Bool
balanced (Leaf n) = True 
balanced (Node l r) 
  | abs (leaf l - leaf r) <= 1 = True
  | otherwise                  = False

leaf :: Tree -> Int 
leaf (Leaf n) = 1
leaf (Node l r) = leaf l + leaf r


balance :: [Int] -> Tree
balance [] = Leaf 0
balance [n] = Leaf n
balance xs = Node (balance (take half xs)) (balance (drop half xs))
  where half = length xs `div` 2
