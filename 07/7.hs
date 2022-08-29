type Bit = Int

-- unfold
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

-- chop8 : ビット列を8ビットずつに区切る
chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

-- map f
mymap :: (a -> b) -> [a] -> [b]
mymap f = unfold null (f . head) (drop 1)


-- iterate f
myiterate :: (a -> a) -> a -> [a]
myiterate f = unfold (const False) id f