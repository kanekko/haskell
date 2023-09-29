---- 31. Funciones de Orden Superior
-- Ejemplo 1:
-- ghci> map odd [1..5]

-- Ejemplo 2:
-- :m Data.List
-- ghci> (reverse . sort) [5,3,5,2]

-- Ejemplo 3:
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- ghci> applyTwice sqrt 16


---- 32. Funciones Anónimas
-- Ejemplo 1 (Función sin nombre):
-- \x -> x + 3
-- (\x -> x + 3) 4

-- Ejemplo 2 (Función con nombre):
doble x = 2 * x
-- ghci> doble 3
-- ghci> map doble [1,2,3]
-- ghci> map (\x -> 2 * x) [1,2,3]


---- 33. Secciones
-- Ejemplo 1:
dobleSecciones = (* 2)

-- Ejemplo 2:
-- ghci> map (* 2) [1,2,3]

-- Ejemplo 3:
mitad = (/ 2)

-- Ejemplo 4:
esMayuscula = (`elem` ['A'..'Z'])
-- ghci> esMayuscula 'b'