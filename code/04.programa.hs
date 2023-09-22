factorial :: Integer -> Integer


factorial 0 = 1 -- caso base
factorial n = n * factorial (n-1) -- caso recursivo

doblar x = 2 * x
