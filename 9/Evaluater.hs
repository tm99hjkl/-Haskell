module Evaluater where
import Parsing ( Parser, parse, natural, symbol )
import Control.Applicative ( Alternative((<|>)) )

-------------------------------------------------
-- 式のパーサー                                 --
-- 式の構造をBNF記法で書くと，以下のようになる．  --
--   expr   ::= term ('+' expr | '-' expr | x) --
--   term   ::= pow ('*' term | '/' term | x)  --
--   pow    ::= factor ('^' pow | x)           --
--   factor ::= '(' expr ')' | nat             --
--   nat    ::= [0-9]+                         --
-- これらをHaskell流に書き下してゆけばよい       --
-------------------------------------------------

-- eval : 文字列として数式を受け取って，解析・計算する
eval :: String -> Int
eval xs = case parse expr xs of
  [(n, "")] -> n
  [(_, out)] -> error ("unused input " ++ out)
  [] -> error "invalid input poyopoyo"


expr :: Parser Int
expr =
  do
    t <- term
    do
      symbol "+"
      e <- expr
      return (t + e)
      <|> do
        symbol "-"
        e <- expr
        return (t - e)
      <|> do
        return t

term :: Parser Int
term =
  do
    p <- pow
    do
      symbol "*"
      t <- term
      return (p * t)
      <|> do
        symbol "/"
        t <- term
        return (p `div` t)
      <|> do
        return p

pow :: Parser Int
pow =
  do
    f <- factor
    do
      symbol "^"
      p <- pow
      return (f ^ p)
      <|> do
        return f

factor :: Parser Int
factor =
  do
    symbol "("
    e <- expr
    symbol ")"
    return e
    <|> natural

