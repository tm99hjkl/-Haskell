import Q8

transmit2 :: String -> String
transmit2 = decode . tail . encode