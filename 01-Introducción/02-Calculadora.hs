-- Operaciones básicas
sumar :: Integer -> Integer -> Integer
sumar x y = x + y

restar :: Integer -> Integer -> Integer
restar x y = x - y

multiplicar :: Integer -> Integer -> Integer
multiplicar x y = x * y

dividir :: Integer -> Integer -> Integer
dividir x y =  div x y


-- Otro tipo de operaciones
doble :: Integer -> Integer
doble x = 2 * x

cuadrado :: Integer -> Integer
cuadrado x = x * x

absoluto :: Integer -> Integer
absoluto n = if n>=0 then n else (-n)

perimetro :: Integer -> Integer -> Integer
perimetro base altura = doble (base + altura)