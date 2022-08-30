module Main(main) where

import Control.Concurrent


main :: IO ()
main = life1 glider

---------------------------
-- ライフゲームの実行部分 --
---------------------------
life1 :: Board -> IO ()
life1 b = do cls
             showcells b
             threadDelay 100000
             life1 (nextgen b)
             

life2 :: Board -> IO ()
life2 b = do showcells b
             threadDelay 100000
             delcells dif
             life2 (nextgen b)
  where
    dif = [cell | cell <- b, cell `notElem` nextgen b]


delcells :: Board -> IO ()
delcells b = seqn [writeat p " "| p <- b]


wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n]]

cls :: IO ()
cls = goto (1,height + 1) >> putStr "\ESC[2J"


--------------------------
-- IO系のインタフェース
--  seqn, goto, writeat
--------------------------

-- seqn : リストとしてIOアクションを受け取って，逐次実行するIO()
--        を生成する関数．
seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) =
  do
    x
    seqn xs

-- Pos : 画面上の座標を表す型
type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) =
  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat (x, y) str = do
  goto (x, y)
  putStr str

-----------------------------
-- ライフゲームのボード定義 --
-----------------------------

-- Board: ライフゲームのボード用の型．
--        生きているセルの座標のリスト．
type Board = [Pos]

-- (board example) glider
glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

-- 高さと幅
width :: Int
width = 10
height :: Int
height = 10

-- showcells : 生きているセルを画面上に表示するための関数
showcells :: Board -> IO ()
showcells b = seqn [writeat p "." | p <- b]

-- makecell : 与えられた座標にセルを打つ
{- 
makecell :: IO Board
makecell = 
  do
    putStr "x: "
    x <- getLine
    putStr "y: "
    y <- getLine
    showcells [(read x, read y)]
    goto (1, height + 2 + 2)
    putStr "continue? (Y: \\n, N: q)"
    c <- getLine 
    p <- return (read x, read y)
    
    if null c then do 
-}

-------------------------------------------------
-- セルの生死など，ライフゲームのルール用の関数群 --
--  isAlivem isEmpty neighbs liveneighbs       --
--  survivors, births, nextgen                 --
-------------------------------------------------

-- isAlive : 指定したセルが生きているかどうか判定する関数
isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

-- isEmpty : 指定したセルが死んでいるか判定
isEmpty :: Board -> Pos -> Bool
isEmpty b p = not $ isAlive b p

-- neighbs : 指定した座標の周囲の座標のリストを返す関数
--           modは関数なので，-1 `mod 5では，
--           1 `mod` 5が先に実行されて，-1が返る．
--           x `mod` 5 (x <- -1)の場合，(x) `mod` 5．
--           となり結果が異なる．
neighbs :: Pos -> [Pos]
neighbs (x, y) =
  map
    wrap
    [ (x - 1, y - 1),
      (x    , y - 1),
      (x + 1, y - 1),
      (x - 1, y    ),
      (x + 1, y    ),
      (x - 1, y + 1),
      (x    , y + 1),
      (x + 1, y + 1)
    ]
  where
    wrap (x, y) = (((x - 1) `mod` width) + 1, ((y - 1) `mod` height) + 1)

-- liveneighbs : 与えられたセルの周囲の生きているセルの個数をカウント
liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

-- survivors : 生きているセルが次の世代でも生きているか判定
--             (周囲に2または3セルが生きていれば生存)
survivors :: Board -> [Pos]
survivors b = [p | p <- b, liveneighbs b p `elem` [2, 3]]

-- births : 次の世代に誕生するセルの座標のリストを返す．
--          (死んでいたセルの周囲に3セルあれば誕生)
births :: Board -> [Pos]
births b =
  [ p | p <- rmdups (concatMap neighbs b), isEmpty b p, liveneighbs b p == 3]

-- rmdups : リストから重複要素を取り除く関数
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

-- nextgen : 次の世代のボード
nextgen :: Board -> Board
nextgen b = survivors b ++ births b
