chain :: (Integral a) => a ->[a]
chain 1 = [1]
chain n
    | odd n = (3*n+1):chain (3*n+1)
    | even n = (div n 2):chain (div n 2)

getLimitChain :: (Integral a) =>Int -> a -> [a]
getLimitChain a b = take a (chain b)

