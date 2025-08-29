{-- 1. Expresiones --}
1 + 2 * 3  -- 7

(1 + 2) * 3 -- 9

even 10 -- True

even (10) -- Parentesis no necesarios

even "José" -- Error

div 6 2 -- 3


{-- 2. Tipos --}
:type 'R'
-- 'R' :: Char
:type "José"
-- "José" :: String
:type not
-- not :: Bool -> Bool
:type length
-- length :: Foldable t => t a -> Int


{-- 3. Factorial --}
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
-- ejecución:
-- $ factorial 5
-- $ map factorial [0..5]


{-- 4. Quicksort --}
quicksort [] = []                                                  -- caso base
quicksort (x:xs) = (quicksort menors) ++ [p] ++ (quicksort majors) -- caso recursivo
    where
        menors = [x | x <- xs, x < p]
        majors = [x | x <- xs, x >= p]
-- ejecución:
-- $ :type quicksort 
-- $ quicksort [6, 5, 4, 3, 2, 1]
-- $ quicksort ["josé", "canek", "pepe", "brenda"]


