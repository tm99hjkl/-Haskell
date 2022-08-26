module Parsing where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Char
  ( isAlpha,
    isAlphaNum,
    isDigit,
    isLower,
    isSpace,
    isUpper,
  )



--------------------------------------
-- パーサーを実行するための根幹の関数 --
--------------------------------------

-- parse : パーサーを実行して結果を返す関数
parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp


------------------------------------------------------------
-- ParserをMonadのインスタンスにする．                     --
-- Functor => Applicative => Monadなので，                --
--  Functor, Applicativeのインスタンス定義も必要           --
------------------------------------------------------------

-- Parserをnewtype宣言で定義してMonadのインスタンスとする   --
newtype Parser a = P (String -> [(a, String)])

-- Sequencing parsers
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> [(g v, out)]
      )

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v, inp)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px =
    P
      ( \inp -> case parse pg inp of
          [] -> []
          [(g, out)] -> parse (fmap g px) out
      )

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> parse (f v) out
      )
  return v = P (\inp -> [(v, inp)])

-- Making choices
instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q =
    P
      ( \inp -> case parse p inp of
          [] -> parse q inp
          [(v, out)] -> [(v, out)]
      )



-----------------------------
-- パーサーの小さい部品たち --
-----------------------------

-- failure : パース失敗を表す空リストを返す関数
failure :: Parser a
failure = P (\inp -> [])

-- item : 入力文字列が空なら空リストを返し，それ以外は1文字読み進める
item :: Parser Char
item =
  P
    ( \inp -> case inp of
        [] -> []
        (x : xs) -> [(x, xs)]
    )

-- sat : 引数として受け取ったCharに対する述語を
--       充足していればそのCharを返す．
--       そうでなければfailure
sat :: (Char -> Bool) -> Parser Char
sat p =
  do
    x <- item
    if p x then return x else failure

-- digit : 0~9か解析するパーサー
digit :: Parser Char
digit = sat isDigit

-- lower : 小文字か解析するパーサー
lower :: Parser Char
lower = sat isLower

-- upper : 大文字か解析するパーサー
upper :: Parser Char
upper = sat isUpper

-- letter : a-Zか解析するパーサー
letter :: Parser Char
letter = sat isAlpha

-- alphanum : 英数字か解析するパーサー
alphanum :: Parser Char
alphanum = sat isAlphaNum

-- char : 指定した一文字と一致するかを解析するパーサー
char :: Char -> Parser Char
char x = sat (== x)

-- string : パースする文字列中に，引数の文字列があるかどうか解析
string :: String -> Parser String
string [] = return []
string (x : xs) =
  do
    char x -- sat の定義から，成功すればxがそのままreturnされることが
    --   分かっているので，引数として名前をもらっているxを使い，
    --   char xの結果は局所変数にバインドしていない．
    --   failureが返ると，バインド演算子の定義から，空リストを返して終わる．
    --   失敗した時点で，その後パースが継続されることは無く，終了する．
    string xs -- string についても同様．
    --   stringが実行される対象は，char xの返り値[(v, out)]のout．
    return (x : xs) --

-- many (imported in Control.Applicative)
-- many : 0回以上パーサーを適用する関数．
mymany :: Parser a -> Parser [a]
mymany p = mymany1 p <|> return []

-- many1 : 1回以上のパース成功を要求するmany関数
--
mymany1 :: Parser a -> Parser [a]
mymany1 p =
  do
    v <- p
    vs <- mymany p
    return (v : vs)

-- ident : 識別子を区切るパーサー
ident :: Parser String
ident =
  do
    x <- lower
    xs <- mymany alphanum
    return (x : xs)

-- nat : 自然数用パーサー
nat :: Parser Int
nat =
  do
    num <- mymany1 digit
    return (read num)

-- space : 空白を無視するパーサー
space :: Parser ()
space =
  do
    mymany $ sat isSpace
    return ()

-- token : トークンの前後の空白を切り取る関数
token :: Parser a -> Parser a
token p =
  do
    space -- 出力は捨てるから，何にも束縛しない．
    v <- p
    space
    return v

-- identifier : 識別子をトークンとして切り取るパーサー
identifier :: Parser String
identifier = token ident

-- natural : 自然数をトークンとして切り取るパーサー
natural :: Parser Int
natural = token nat

-- symbol : 特定の文字列のトークンを切り取るパーサー
symbol :: String -> Parser String
symbol xs = token (string xs)

-- nl : Haskellの整数リスト解析器っぽいやつ
nl :: Parser [Int]
nl =
  do
    symbol "["
    n <- natural
    ns <-
      mymany
        ( do
            symbol ","
            natural
        )
    symbol "]"
    return (n : ns)
