module Text where
import Data.Char ( ord, chr )


type Bit = Int

-- bit2int [1,0,1,1] = 13
-- bit2int bs = sum [b * w | (b, w) <- zip bs ws]
--   where ws = iterate (*2) 1
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0


-- int2bin
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = (n `mod` 2) : int2bin (n `div` 2)


-- make8
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- encode
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)


-- chop8
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)


-- decode
decode :: [Bit] -> String 
decode = map (chr . bin2int) . chop8


-- transmit
transmit :: String -> String 
transmit = decode . channel . encode
  where
      channel :: [Bit] -> [Bit]
      channel = id