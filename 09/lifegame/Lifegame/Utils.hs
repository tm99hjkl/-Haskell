module Lifegame.Utils where


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
