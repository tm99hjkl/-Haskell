-- curry
mycurry :: ((a, b) -> c) -> (a -> b -> c)
mycurry f = g
  where
    g a b = f (a, b)

-- uncurry
myuncurry :: (a -> b -> c) -> ((a, b) -> c)
myuncurry f = g
  where
    g (a, b) = f a b