{- 
    Definición de funciones
-}
factorialA :: Integer -> Integer    -- Cabecera o firma
factorialA 0 = 1                    -- Definición
factorialA n = n * factorialA (n-1) -- Definición

facttorialB :: Integer -> Integer                           -- Cabecera o firma
facttorialB n = if n == 0 then 1 else n * facttorialB (n-1) -- Definición

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)


{-
    Definición de una función con patrones
-}
nand :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = True


{-
    Definición de una función con guardas
-}
valorAbsoluto :: Integer -> Integer
valorAbsoluto n
        | n>=0      = n
        | otherwise = -n


{-
    Definición Local de una función 
-}
{-fastExpA :: Integer -> Integer -> Integer
fastExpA _ 0 = 1
fastExpA x n =
    let y = fastExpA x n_halv
        n_halv = div n 2
    in
        if even n
        then y * y
        else y * y * x-}
fastExpA :: Integer -> Integer -> Integer
fastExpA _ 0 = 1
fastExpA x n =
  let y1 = fastExpA x n_halv_a
      y2 = fastExpA x n_halv_b
      n_halv_a = div n 2
      n_halv_b = div (n - 1) 2
   in if even n
        then y1 * y1
        else y2 * y2 * x


fastExpB :: Integer -> Integer -> Integer
fastExpB _ 0 = 1
fastExpB x n
    | even n = y * y
    | otherwise = y * y * x
    where
        y = fastExpB x n_halv
        n_halv = div n 2


calculateArea :: Double -> Double -> Double
calculateArea length width =
  let area = length * width
      perimeter = 2 * (length + width)
   in area + perimeter


{-
Otros ejemplos
-}
doubleMe x = x + x

-- doubleUs x y = x*2 + y*2
doubleUs x y = doubleMe x + doubleMe y


lucky :: (Integral a) => a -> String
lucky 7 = "¡El siete de la suerte!"
lucky x = "Lo siento, ¡no es tu día de suerte!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "¡Uno!"
sayMe 2 = "¡Dos!"
sayMe 3 = "¡Tres!"
sayMe 4 = "¡Cuatro!"
sayMe 5 = "¡Cinco!"
sayMe x = "No entre uno 1 y 5"

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"


{-
Listas
-}
-- ghci> let lostNumbers = [4,8,15,16,23,42]
-- ghci> [1,2,3,4] ++ [9,10,11,12]
-- ghci> "hello" ++ " " ++ "world"
-- ghci> ['h','o'] ++ ['l','a']

sumarLista :: [Int] -> Int
sumarLista [] = 0
sumarLista (h:t) = h + sumarLista t



