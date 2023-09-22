module Practica1 where

{-----------------------------------------------------------------------------------
-------------------------            PRACTICA 1            -------------------------
-------------------------             NATURALES            -------------------------
------------------------------------------------------------------------------------}

{------------------------------------------------------------------------------------
Se genera un tipo de dato de la forma S...S Cero que indica los sucesores del cero 
usando la función sucesor S como en la artimética de Peano. 
Se desarrolla la artimética de Peano para los naturales.
------------------------------------------------------------------------------------}

data Natural = Cero | S Natural deriving Show

-- Ejercicio 1: recibe un número entero y regresa su valor equivalente de tipo Natural.
-- Ejemplo a_natural 3 -> S (S (S Cero))
a_natural :: Int -> Natural
a_natural 0 = Cero
a_natural n = if n<0 then error "no existen los números naturales." else S (a_natural (n-1))

-- Ejercicio 2: regresa el valor entero equivalente del parámetro.
-- Ejemplo a_entero (S (S (S Cero))) -> 3
a_entero :: Natural -> Int
a_entero Cero = 0
a_entero (S n) = 1 + a_entero n

-- Ejercicio 3: regresa la suma de los valores de tipo Natural.
-- Ejemplo suma_nat (S (S (S Cero))) (S (S Cero)) -> S (S (S (S (S Cero))))
suma_nat :: Natural -> Natural -> Natural
suma_nat n Cero = n
suma_nat n (S m) = suma_nat (S n) m

-- Ejercicio 4: regresa la multiplicación de los valores de tipo Natural.
mult_nat :: Natural -> Natural -> Natural
mult_nat Cero _ = Cero
mult_nat _ Cero = Cero
mult_nat n (S Cero) = n
mult_nat n (S m) = suma_nat n (mult_nat n m)


-- Ejercicio 5: regresa la resta de los valores de tipo Natural.
resta_nat :: Natural -> Natural -> Natural
resta_nat Cero Cero = Cero
resta_nat Cero _ = error "no regresa un numero natural."
resta_nat n Cero = n
resta_nat (S n) (S m) = resta_nat n m

-- Ejercicio 6: regresa el valor True en caso que ambos parámetros sean iguales, regresa el valor False en cualquier otro caso.
igual :: Natural -> Natural -> Bool
igual Cero Cero = True
igual Cero _ = False
igual _ Cero = False
igual (S n) (S m) = igual n m

-- Ejercicio 7: regresa el valor True en caso que el valor del primer parámetro sea menor que el valor del segundo parámetro, regresa el valor False en cualquier otro caso.
menor_que :: Natural -> Natural -> Bool
menor_que Cero Cero = False
menor_que Cero _ = True
menor_que _ Cero = False
menor_que (S n) (S m) = menor_que n m

-- Ejercicio 8: regresa el valor True en caso que el valor del primer parámetro sea mayor que el valor del segundo parámetro, regresa el valor False en cualquier otro caso.
mayor_que :: Natural -> Natural -> Bool
mayor_que Cero Cero = False
mayor_que Cero _ = False
mayor_que _ Cero = True
mayor_que (S n) (S m) = mayor_que n m




-- Ejercicio 9: regresa el factorial del valor del parámetro.
factorial :: Natural -> Natural
factorial Cero = S Cero
factorial (S n) = mult_nat (S n) (factorial n)

-- Ejercicio 10: regresa el número de Fibonacci del valor del parámetro.
fibonacci :: Natural -> Natural
fibonacci Cero = Cero
fibonacci (S Cero) = S Cero
fibonacci (S (S n)) = suma_nat (fibonacci (S n)) (fibonacci n)

-- Ejercicio 11: regresa el valor mínimo de una lista de valores de tipo Natural.
minimo :: [Natural] -> Natural
minimo [] = error "no hay elemento minimo en una lista vacia."
minimo (x:[]) = x
minimo (x:xs) = if menor_que x (minimo xs) then x else minimo xs

-- Ejercicio 12: regresa el valor máximo de una lista de valores de tipo Natural.
maximo :: [Natural] -> Natural
maximo [] = error "no hay elemento maximo en una lista vacia."
maximo (x:[]) = x
maximo (x:xs) = if mayor_que x (maximo xs) then x else maximo xs

-- Ejercicio 13: regresa el valor True en caso que el valor del parámetro sea un número par, regresa el valor False en cualquier otro caso.
par :: Natural -> Bool
par Cero = True
par (S Cero) = False
par (S (S n)) = par n

--Ejercicio 14: regresa el valor True en caso que el valor del parámetro sea un número impar, regresa el valor False en cualquier otro caso.
impar  :: Natural -> Bool
impar Cero = False
impar (S Cero) = True
impar (S (S n)) = impar n
