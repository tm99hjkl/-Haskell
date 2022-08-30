module Lifegame.Board (Pos, Board) where


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
-- 高さと幅
width :: Int
width = 10
height :: Int
height = 10


-- (board example) glider
glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]


-- showcells : 生きているセルを画面上に表示するための関数
showcells :: Board -> IO ()
showcells b = seqn [writeat p "." | p <- b]

