maxIdx :: (Ord a, Integral b) => [a] -> b
maxIdx [] = 0
maxIdx xs = let ys = zip (take (length xs) [1,2..]) xs in fst (maxIdx_ ys)

maxIdx_ :: (Integral a, Ord b) => [(a, b)] -> (a,b)
maxIdx_ [(x,b)] = (x,b)
maxIdx_ (pair:xs)
    | snd pair > snd ws = pair
    | otherwise = ws
    where ws = maxIdx_ xs

{-myComp :: (Ord a) => a ->a -> Bool-}
myComp a b
    | (compare a b == GT) = True
    | (compare a b == LT) = False
    | (compare a b == EQ) = True
quickSort :: (Ord a, Integral b, Enum b) => [a]-> (a -> a -> Bool) -> [(b, a)]
quickSort [] _ = []
quickSort [x] _  = [(1,x)]
quickSort xs comp = let ys = wrap xs in
                    let ts = quickSort_ ys comp in
                    let ms = wrap ts in
                    {-let newComp :: (Ord b, Ord a) -> (b,a) -> (b,a) -> Ordering-}
                    let newComp a b = myComp (fst a) (fst b) in
                    let ds = quickSort_ ms newComp
                    in restore ds
quickSort_ :: (Ord a, Integral b) => [(b, a)] -> (a -> a -> Bool) -> [(b, a)]
quickSort_ [] _ = []
quickSort_ [pa] _ = [pa]
quickSort_ (pa:xs) comp = let smallArray = [pair | pair <- xs, not (comp (snd pair) (snd pa))] in
                         let bigArray = [pair | pair <- xs, comp (snd pair) (snd pa)] in
                         (quickSort_ smallArray comp) ++ [pa] ++ (quickSort_ bigArray comp)

wrap :: (Enum b, Integral b) => [a] -> [(b,a)]
{-wrap xs = zip (take(length xs) [1,2..]) xs-}
wrap xs = zip [1,2..] xs

restore :: [(a,(b,c))] -> [(a,c)]
restore [] = []
restore [(a,(b,c))] = [(a,c)]
restore ((a,(b,c)):xs) = (a,c):(restore xs)

