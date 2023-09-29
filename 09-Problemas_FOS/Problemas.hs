---- 34. Problema 1
eql :: [Int] -> [Int] -> Bool
eql x y
    | length x /= length y = False
    | otherwise = and $ zipWith (==) x y