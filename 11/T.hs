-- 切符番号遊び

-- 式中で使われる演算子を表すデータ型Op
data Op = Add | Sub | Mul | Div
  deriving (Show)


-- 式を表すデータ型Expr
data Expr = Val Int | App Op Expr Expr
  deriving (Show)


-- 用いられる演算が有効か否かを判定する関数
-- 代数的性質を用いてvalidの条件を厳しくすることで，
-- 計算量を減らすことを考えている．
--  交換法則より，和と積の引数が昇順であることを要求できる
--  乗除法の単位元で計算結果が変わらないことより，単位元でないことを要求できる．
valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0

{-
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0
-}


-- 演算子を適用する関数
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y


-- ? values : 式の中の数値をリストとして返す関数
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r


-- eval: 式全体の値を返す関数
--       空リストは評価失敗を意味する
--       Maybeでも可能
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]


-- リストに関するいくつかの関数
-- subs : リストを受け取るとべき集合を返す関数
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = yss ++ map (x:) yss
  where yss = subs xs


-- interleave : リストの各要素の間に，与えられた要素を挿入したリストを返す関数
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x:y:ys) : map (y:) (interleave x ys)


-- perms : リストを受け取ると，その要素の全順列を返す関数
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concatMap (interleave x) (perms xs)


-- choices : 与えられたリストに対して，元を0個以上取り出したときの全てに対する
--           順列を返す
choices :: [a] -> [[a]]
choices xs = concatMap perms (subs xs)


-- solutions : 与えられた式が，切符番号遊びの制約で与えられた
--             数値のみからなり，その結果が答えと合致するか判定する関数
solution :: Expr -> [Int] -> Int -> Bool 
solution e ns n = values e `elem` choices ns && eval e == [n]


-- split : リストを受け取って，空でない2つのリストに分割した組の全てのパターンを返す関数
-- idea  : 再帰を使うことを考える．出力結果のfstのheadを取り除くと，それはsplit xsであることを利用．
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

-- exprs : 数値のリストを受け取って，それを用いて生成され得る全ての式を返す関数
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]


solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns,
                      e <- exprs ns',
                      eval e == [n]]


-- 式と，その式が持つ値との組を表す型シノニムResult
type Result = (Expr, Int)

-- results: 与えられた整数達を使って，有効な計算とその結果の組全てを返す関数
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
                    lx <- results ls,
                    ry <- results rs,
                    res <- combine' lx ry]
                    

-- combine': 受け取った二つの式に対して，全ての演算子を適用した式とその結果を返す関数
combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops,
                                                     valid o x y]


-- solutions': 無効な式を省いて計算したもの．
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns,
                       (e, m) <- results ns', 
                       m == n]