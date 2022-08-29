or0 :: Bool -> Bool -> Bool
True  `or0` True  = True 
True  `or0` False = True
False `or0` True  = True 
False `or0` False = False 

or1 :: Bool -> Bool -> Bool 
False `or1` False = False 
_     `or1` _     = True

or2 :: Bool -> Bool -> Bool 
False `or2` b = b
True  `or2` _ = True 

or3 :: Bool -> Bool -> Bool 
b `or3` c | b == c     = b
          | otherwise  = True