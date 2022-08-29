and0 :: Bool -> Bool -> Bool 
b `and0` c =
    if b then
        if c then True 
        else False
    else False