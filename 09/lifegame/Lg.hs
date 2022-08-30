module Main(main) where

import Control.Concurrent
import Lifegame.Utils
import Lifegame.Board


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

