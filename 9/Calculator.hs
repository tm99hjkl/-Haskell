import Parsing ( parse )  -- evalを再定義するためにparse自体が必要なので．
import Evaluater ( expr )
import System.IO ( hSetEcho, stdin )

-------------------------------------------------------------------------
-- 電卓の実行部分                                                       --    
--  run                                                                --
-- 処理概要 : コンソールの画面クリア → 電卓表示 → 電卓の画面クリア(ループ) --
-------------------------------------------------------------------------
run :: IO ()
run = do cls
         showbox
         clear



--------------------------
-- 描画のベース関数達    --
--  type: Pos           --
--  func: goto, writeat -- 
--------------------------

-- Pos : 画面上の座標を表す型
type Pos = (Int, Int)

-- goto : コンソール画面上で，指定した座標にカーソルを移動する関数
goto :: Pos -> IO ()
goto (x, y) =
  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- writeat : 指定した座標から文字列を書き始める関数
writeat :: Pos -> String -> IO ()
writeat (x, y) str = do
  goto (x, y)
  putStr str



-----------------------------------
-- IO部品系                      --
--  func: getCh, beep, cls, seqn --
-----------------------------------

-- getCh : 入力した文字をコンソール画面に表示することなく文字を取得するための関数
--         (この機能いるのか？)
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

-- beep : ビープ音を鳴らす
beep :: IO ()
beep = 
  do
    putStr "!!"
    wait 5000000
    putStr "\BS\BS"
    putStr "  "

wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n]]
-- cls : 画面をクリアする
cls :: IO ()
cls = putStr "\ESC[2J"

-- seqn : リスト内のIOアクションを順に実行(do)していく　というひとつのIOアクションを作る．
--         結果は捨てる．
--         showbox関数で使う．
seqn :: [IO a] -> IO ()
seqn [] = return  ()
seqn (a : as) =
  do
    a
    seqn as



-----------------------------------------
-- 電卓の内部仕様を実装する関数          --
-- var: box, buttons                   -- 
-- func: showbox, display, calc        -- 
-- subf: process, quit, delete, eval,  --
--       clear, press                  --  
-----------------------------------------

-- box : 電卓の画面
box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]
       
-- buttons : 電卓で使用できるボタンのリスト
buttons :: [Char]
buttons = standard ++ extra
  where
    standard = "qcd=123+456-789*0()/"
    extra    = "QCD \ESC\BS\DEL\n"

-- showbox : 電卓を画面の左上の角に表示するための関数
--           改行が座標で表されていてすごい
--           guiで実装する場合は，1ボタンの大きさピクセル分プラスしたようなリストかな
showbox :: IO ()
showbox = 
  do
    seqn $ [writeat (1, y) xs | (y, xs)  <-zip [1..13] box]
    putStr "\n"

-- display : 計算結果を電卓の画面に表示する関数
display :: String -> IO ()
display xs = 
  do
    writeat (3, 2) "             "
    writeat (3, 2) (reverse $ take 13 $ reverse xs)  -- ケツから13文字分取得
    goto(1,14)

-- calc : 電卓での実際の計算を行う
calc :: String -> IO ()
calc xs =
  do
    display xs
    c <- getCh  -- ここは1文字しか取得しないから，文字列を入力するとバッファに行く
    if c /= '\n' then 
      getCh
      else
        do
        return '\a'
    if c `elem` buttons then
      process c xs
      else
        do
          beep
          calc xs

process :: Char -> String -> IO ()
process c xs
  | c `elem` "qQ\ESC"    = quit
  | c `elem` "dD\BS\DEL" = delete xs
  | c `elem` "=\n"       = Main.eval xs
  | c `elem` "cC"        = clear
  | otherwise            = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

-- TODO : エラー発生後に空白文字列をwriteatする良い方法
--        errormsglenを定義した方がいいかもね
eval :: String -> IO ()
eval xs = 
  case parse expr xs of
    [(n, "")] -> calc (show n) -- パーサーが評価し終わったら　という意味
    [(_, out)] -> do putStr (err ++ out)
                     getCh
                     putSpace (length (err ++ out))
                     calc ""
    _ -> do beep
            calc xs
  where
    err = "error occured : "
    putSpace n = writeat (1,14) $ replicate n ' '
    

clear :: IO ()
clear = calc ""

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])
