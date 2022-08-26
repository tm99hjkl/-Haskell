module T where
{-
 type は既存の型を組み合わせて新たな方を作る
 data は，その型が取りうる値を全て指定することで
      型を作る．この値のことを"構成子"という
      ex) data Bool = False | True
          「Boolという型は，FalseまたはTrueという値(-構成子)を持つ」

     取りうる値を制限して意味を持たせられるのが良いのかな
-}


-- 座標を表す型シノニムPos
type Pos = (Int, Int)

-- 移動する方向を表す代数的データ型Move
data Move = Righ | Lef | U | Dow

move :: Move -> Pos -> Pos
move Righ (x, y) = (x + 1, y)
move Lef (x, y) = (x - 1, y)
move U (x, y) = (x, y + 1)
move Dow (x, y) = (x, y - 1)

mflip :: Move -> Move
mflip Righ = Lef
mflip Lef = Righ
mflip U = Dow
mflip Dow = U


-- 形を表す代数的データ型Shape
data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float 
area (Circle r) = pi * r^2
area (Rect x y) = x * y


-- 概ねそうなることを表すデータ型Maybe
-- Maybe = Nothing | Just a

safediv :: Int -> Int -> Maybe Int 
safediv _ 0 = Nothing 
safediv n m = Just (n `div` m)

safehead :: [a] -> Maybe a
safehead [] = Nothing 
safehead (x : xs) = Just x


-- 自然数を表すデータ型Nat
data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n 

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ n) m = Succ (add n m)

add2 :: Nat -> Nat -> Nat
add2 n Zero = n
add2 n (Succ m) = Succ (add2 n m)

mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult m (Succ n) = add m (mult m n)

-- リストを表すデータ型List
data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs


-- 二分木を表すデータ型Tree
data Tree = Leaf Int | Node Tree Int Tree
  deriving (Show)
t :: Tree
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

s :: Tree
s = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 8))


occurs :: Int -> Tree -> Bool
occurs n (Leaf m) = n == m
occurs n (Node lt m rt) = n == m || occurs n lt || occurs n rt

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node lt m rt) = flatten lt ++ [m] ++ flatten rt

occurs2 :: Int -> Tree -> Bool
occurs2 n (Leaf m) = n == m
occurs2 n (Node l m r)
  | n == m    = True
  | n < m     = occurs2 n l
  | otherwise = occurs2 n r

occurs3 :: Int -> Tree -> Bool 
occurs3 n (Leaf m) = 
  case compare n m of
    EQ -> True
    _  -> False
occurs3 n (Node lt m rt) =
  case compare n m of
    EQ -> True
    LT -> occurs3 n lt
    GT -> occurs3 n rt


  

-- 論理式を記述するためのデータ型Prop
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | Equ Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- 連想配列用の型シノニムAssoc
type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v' | (k', v') <- t, k == k']

-- 論理変数に対する真理値割り当て表を表す型シノニムSubst
type Subst = Assoc Char Bool

-- eval : 論理式と真理値割り当てを受け取ったら評価する関数
eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var p) = find p s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p &&  eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equ p q) = (eval s p <= eval s q) && (eval s p >= eval s q)

-- vars : 論理式から論理変数を抜き出す関数
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var p) = [p]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

-- bools : 2^(論理変数の個数)通りの真理値のリストを生成する関数
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
  where bss = bools (n-1)

-- substs : 受け取った論理式に対して，その論理変数のとりうる
--          全ての真理値割り当て表を返す関数．
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = rmdups $ vars p


-- rmdups : リストから重複を取り除く関数
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups ([y | y <- xs, y /= x])
            

-- isTaut : 与えられた命題が恒真式かどうか判定する関数
--          (綺麗)
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]


-- 単純な式を表すデータ型Expr
data Expr = Val Int | Add Expr Expr

-- value : Expr型で表された式がIntでどのような値を持つか計算する関数
value :: Expr -> Int
value (Val n) = n
value (Add e1 e2) = value e1 + value e2 


-- 仮想マシンを制御するスタックの型
type Cont = [Op]
data Op = EVAL Expr | ADD Int 

-- 
evalu :: Expr -> Cont -> Int
evalu (Val n) c = exec c n
evalu (Add x y) c = evalu x (EVAL y  :c)

exec :: Cont -> Int -> Int 
exec [] n = n
exec (EVAL y : c) n = evalu y (ADD n  :c)
exec (ADD n : c) m = exec c (n + m)


