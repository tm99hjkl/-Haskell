rep :: Int -> a -> [a]
rep iter elem = [elem | _ <- [1..iter]]