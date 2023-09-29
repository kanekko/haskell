---- 34. Problema 1
eql :: [Int] -> [Int] -> Bool
eql x y
    | length x /= length y = False
    | otherwise = and $ zipWith (==) x y

---- 35. Problema 2
prod :: [Int] -> Int
prod = foldl (*) 1

---- 36. Problema 3
prodOfEvens :: [Int] -> Int
prodOfEvens = prod . filter even

