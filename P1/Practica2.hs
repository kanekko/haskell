module Practica2 where

------------------------------------------------------------------------------------
-------------------------            PRACTICA 2            -------------------------
------------------------        FUNCIONES RECURSIVAS        ------------------------
------------------------------------------------------------------------------------

-- Ejercicio 1: Función recursiva para obtener los números de Fibonacci
fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

-- Ejercicio 2: Función para número triangular
num_triangular :: Int -> Int
num_triangular 0 = 0
num_triangular n = n + num_triangular (n-1)

-- Ejercicio 3: Función recursiva del producto de dos enteros.
multiplica :: Int -> Int -> Int
multiplica 0 _ = 0
multiplica _ 0 = 0
multiplica n 1 = n
multiplica n m = n + multiplica n (m-1)

-- Ejercicio 4: Función para la potencia
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia n m = multiplica n (potencia n (m-1))

-- Ejercicio 5: Función para el factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n = multiplica n (factorial (n-1))

--Ejercicio 6: Algoritmo de la división 
--Función que aumenta en 1 el primer término
sub :: (Int, Int) -> (Int, Int)
sub (x, r) = (x+1, r)

--Función para el algoritmo de la división
divide :: Int -> Int -> (Int, Int)
divide a b = if a < b then (0, a) else sub (divide (a-b) b)



-- EJERCICIOS LISTAS

-- Ejercicio 1: Regresa el tope de la lista.
-- Si el parámetro es la lista vacía, entonces regresa un mensaje de error.
top :: [a] -> a
top [] = error "La lista está vacía."
top (x:xs) = x

-- Ejercicio 2: Regresa el último elemento de la lista.
-- Si el parámetro es la lista vacía, entonces regresa un mensaje de error.
bot :: [a] -> a
bot [] = error "La lista está vacía."
bot (x:[]) = x
bot (x:xs) = bot xs

-- Ejercicio 3: Longitud de lista.
len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

-- Ejercicio 4: Suma de los elementos de la lista.
suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs

--Ejercicio 5: Invierte la lista.
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = (reversa xs)++[x]

-- Ejercicio 6: Regresa True si el elemento está en la lista, y False en otro caso
pertenece :: (Eq a) => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e==x || pertenece e xs 



-- EJERCICIOS ÁRBOLES

-- Definicion del tipo de dato Arbol binario
-- La creación de un nodo tree(Vacio, n, Vacio) debe hacerse como Nodo "nombre del nodo" Vacio Vacio
-- Dado dos árboles t1, t2 se crea un nuevo árbol como Nodo "nombre" t1 t2
data ArbolB a = Vacio | Nodo a (ArbolB a) (ArbolB a) deriving (Eq, Show)

-- Ejercicio 1: Regresa la raíz del árbol
raiz :: ArbolB a -> a
raiz Vacio = error "El arbol es vacio."
raiz (Nodo e l r) = e

-- Ejercicio 2: Función que calcula el número de nodos en un árbol
num_elementos :: ArbolB a -> Int
num_elementos Vacio = 0
num_elementos (Nodo _ l r) = 1 + (num_elementos l) + (num_elementos r)

-- Ejercicio 3: Función para contar el número de hojas del árbol
num_hojas :: ArbolB a -> Int
num_hojas Vacio = 0
num_hojas (Nodo _ Vacio Vacio) = 1
num_hojas (Nodo _ l r) = num_hojas l + num_hojas r

-- Ejercicio 4: Función que cuenta el número de nodos internos de un árbol (no raiz ni hojas)
internos :: ArbolB a -> Int
internos Vacio = 0
internos a = (i_aux a) - 1

-- Función aux a ejercicio 4.
i_aux :: ArbolB a -> Int
i_aux Vacio = 0
i_aux (Nodo _ Vacio Vacio) = 0
i_aux (Nodo _ l r) = 1 + (i_aux l) + (i_aux r)

-- Ejercicio 5: Función para la profundidad de un árbol.
depth :: ArbolB a -> Int
depth Vacio = 0
depth (Nodo _ l r) = 1 + max (depth l) (depth r)

-- Ejercicio 6: Transforma los nodos del árbol en lista
to_list :: ArbolB a -> [a]
to_list Vacio = []
to_list (Nodo e l r) = e:(to_list l++to_list r)
