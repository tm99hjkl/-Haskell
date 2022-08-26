import Control.Applicative ( Alternative((<|>)) )
import Text


-- int : 整数値を解析するパーサー
int :: Parser Int
int =
  do
    natural
    <|> do
          symbol "-"
          negate <$> natural



-- Q2 --
-- comment : コメントを解析するパーサー
-- コメントは　"--" で始まり，"\n"で終わるものとする
--  疑問：なんでトークンとして区切れているのか．-> symbol のおかげかな
comment :: Parser ()
comment = 
  do
    symbol "--"
    mymany (sat (/= '\n'))
    return ()


-- Q8 --
expr2 :: Parser Int 
expr2 = 
  do
    n <- natural
    ns <- mymany (do symbol "-"
                     natural)
    return (foldl (-) n ns)
                
eval2 :: String -> Int
eval2 xs = case parse expr2 xs of
  [(n, "")] -> n
  [(_, out)] -> error ("unused input " ++ out)
  [] -> error "invalid input"

