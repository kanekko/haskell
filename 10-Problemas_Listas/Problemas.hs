---- 59. Problema 1
ones :: [Integer]
ones = repeat 1

---- 60. Problema 2
nats :: [Integer]
nats = iterate (+1) 0

---- 61. Problema 3
ints :: [Integer]
ints = iterate integers 0
    where
    integers :: Integer -> Integer
    integers x
        | x > 0 = -x
        | otherwise = 1 - x

