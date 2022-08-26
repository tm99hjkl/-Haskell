-- getLine : 1行読み込む
mygetLine :: IO String
mygetLine =
  do
    x <- getChar
    if x == '\n' then
      return []
      else
        do
          xs <- getLine 
          return (x : xs)


-- 
type Pos = (Int, Int)
put :: IO [Pos]
put = 
  do
    x <- getLine 
    y <- getLine
    s <- getLine
    if s /= "q" then 
      do
        ss <- put
        return ((read x, read y) : ss)
    else return [(read x, read y)]