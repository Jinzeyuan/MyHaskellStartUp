twoPow :: (Integral a) => a -> a
twoPow 0 = 1
twoPow a = 2 * twoPow(a-1)

power :: (Integral a) =>a ->a
{-power 0 = 1-}
power n
    | n==0 = 1
    | n<0 = 1
    | otherwise = n * power (n-1)
