import Data.Char
-- encode : シーザー暗号を生成
-- encode 3 "abc" = "def"
encode :: Int -> String -> String 
encode n str = [shift n c | c <- str]


-- shift : 与えられた整数分文字をシフトする
-- shift 3 'a' = 'c'
-- shift -3 'a' = 'x'
-- shift 1 'A' = 'B'
shift :: Int -> Char -> Char
shift n c
    | isUpper c  = int2letU $ (let2intU c + n) `mod` 26
    | isLower c  = int2letL $ (let2intL c + n) `mod` 26
    | otherwise  = c



-- int2letL : to change int to char (Lower case)
-- int2letL 0 = 'a'
-- int2letL 25 = 'z'
int2letL :: Int -> Char
int2letL n = chr (ord 'a' + n)

int2letU :: Int -> Char
int2letU n = chr (ord 'A' + n)

let2intU :: Char -> Int 
let2intU c = ord c - ord 'A'

let2intL :: Char -> Int 
let2intL c = ord c - ord 'a'