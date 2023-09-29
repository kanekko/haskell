module Practica2 where

-- Tipo de dato para lógica proposicional
data LProp = PTrue | PFalse | Var Nombre | Neg LProp | Conj LProp LProp | Disy LProp LProp | Impl LProp LProp | Syss LProp LProp

type Nombre = String
type Asignacion = [(String , Bool)]

-- Ejercicio 0: Visualización del tipo de dato LProp.
-- Las variables se definen como Var "p"
instance Show LProp where
    show PTrue = "True"
    show PFalse = "False"
    show (Var p) = p
    show (Neg p) = "¬" ++ show p
    show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
    show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

-- Ejercicio 1: varsCount
-- vars :: Prop −> Int
-- Cuenta las variables de una proposición.
-- ej: varsCount [Impl (Var "p")  {Conj (Var "q") (Neg (Var "r"))} ]
varsCount :: LProp -> Int
varsCount PTrue = 0
varsCount PFalse = 0
varsCount (Var n) = 1
varsCount (Neg p) = varsCount p
varsCount (Conj p q) = (varsCount p) + (varsCount q)
varsCount (Disy p q) = (varsCount p) + (varsCount q)
varsCount (Impl p q) = (varsCount p) + (varsCount q)
varsCount (Syss p q) = (varsCount p) + (varsCount q)


-- Ejercicio 2: Regresa las variables de una fórmula proposicional.
-- Define recursivamente la variable en una fórmula.
-- vars :: LProp ->[Nombre]
-- vars PTrue = []
-- vars PFalse = []
-- vars (Var n) = [n]
-- vars (Neg p) = vars p
-- vars (Conj p q) = eliminaRepetidos (vars p ++ vars q)
-- vars (Disy p q) = eliminaRepetidos (vars p ++ vars q)
-- vars (Impl p q) = eliminaRepetidos (vars p ++ vars q)
-- vars (Syss p q) = eliminaRepetidos (vars p ++ vars q)

-- eliminaRepetidos :: (Eq a) => [a] -> [a]
-- eliminaRepetidos [] = []
-- eliminaRepetidos (x:xs) = if elem x xs then eliminaRepetidos xs else x:(eliminaRepetidos xs)

-- Captura de argumentos
len ls@(l:_) = "List starts with " ++ show l ++ " and is " ++ show (length ls) ++ " items long."
len [] = "List is empty!"

-- Ejercicio 3: asocia a la derecha en una fórmula proposicional.
-- Por ejemplo, toma como input ( (p v q) v p) y obtiene output ( p v (q v p) ) 
--
-- asocia_der (Disy (Disy (Var "p") (Var "q")) (Var "p"))
-- (p ∨ (q ∨ p))
-- 
asocia_der :: LProp -> LProp
asocia_der a@PTrue = a
asocia_der a@PFalse = a
asocia_der a@(Var n) = a
asocia_der (Neg p) = Neg (asocia_der p)
asocia_der (Conj (Conj a b) c) = Conj (asocia_der a) (Conj (asocia_der b) (asocia_der c))
asocia_der (Conj a b) = Conj (asocia_der a) (asocia_der b)
asocia_der (Disy (Disy a b) c) = Disy (asocia_der a) (Disy (asocia_der b) (asocia_der c))
asocia_der (Disy a b) = Disy (asocia_der a) (asocia_der b)
asocia_der (Impl a b) = Impl (asocia_der a) (asocia_der b)
asocia_der (Syss a b) = Syss (asocia_der a) (asocia_der b)

-- Ejercicio 4: Asocia a la izquierda en una fórmula proposicional. 
-- Por ejemplo, toma como input ( p v (q v p) )  y obtiene output ( (p v q) v p)
-- ghci> asocia_izq ( Disy (Var "p") (Disy (Var "q") (Var "p")) )
-- ((p ∨ q) ∨ p)
asocia_izq :: LProp -> LProp
asocia_izq a@PTrue = a
asocia_izq a@PFalse = a
asocia_izq a@(Var n) = a
asocia_izq (Neg p) = Neg (asocia_izq p)
asocia_izq (Conj a (Conj b c)) = Conj (Conj (asocia_izq a) (asocia_izq b)) (asocia_izq c)
asocia_izq (Conj a b) = Conj (asocia_izq a) (asocia_izq b)
asocia_izq (Disy a (Disy b c)) = Disy (Disy (asocia_izq a) (asocia_izq b)) (asocia_izq c)
asocia_izq (Disy a b) = Disy (asocia_izq a) (asocia_izq b)
asocia_izq (Impl a b) = Impl (asocia_izq a) (asocia_izq b)
asocia_izq (Syss a b) = Syss (asocia_izq a) (asocia_izq b)


-- Ejercicio 5: aplica la propiedad de conmutatividad de forma exhaustiva en una fórmula proposicional.
-- 
conm :: LProp -> LProp
conm a@PTrue = a
conm a@PFalse = a
conm a@(Var n) = a
conm (Neg p) = Neg (conm p)
conm (Conj a b) = Conj (conm b) (conm a)
conm (Disy a b) = Disy (conm b) (conm a)
conm (Impl a b) = Impl (conm a) (conm b)
conm (Syss a b) = Syss (conm a) (conm b)

-- Ejercicio 6: Conmuta las variables en una fórmula proposicional. Por ejemplo p v q = q v p.
dist :: LProp -> LProp
dist a@PTrue = a
dist a@PFalse = a
dist a@(Var n) = a
dist (Neg p) = Neg (dist p)
dist (Conj a (Disy b c)) = Disy (Conj (dist a) (dist b)) (Conj (dist a) (dist c))
dist (Conj (Disy a b) c) = Disy (Conj (dist a) (dist c)) (Conj (dist b) (dist c))
dist (Conj a b) = Conj (dist b) (dist a)
dist (Disy a (Conj b c)) = Conj (Disy (dist a) (dist b)) (Disy (dist a) (dist c))
dist (Disy (Conj a b) c) = Conj (Disy (dist a) (dist c)) (Disy (dist b) (dist c))
dist (Disy a b) = Disy (dist b) (dist a)
dist (Impl a b) = Impl (dist a) (dist b)
dist (Syss a b) = Syss (dist a) (dist b)

-- Ejercicio 7: Aplica las propiedades de DeMorgan en una fórmula proposicional.
deMorgan :: LProp -> LProp
deMorgan a@PTrue = a
deMorgan a@PFalse = a
deMorgan a@(Var n) = a
deMorgan (Neg (Conj a b)) = Disy (Neg (deMorgan a)) (Neg (deMorgan b))
deMorgan (Neg (Disy a b)) = Conj (Neg (deMorgan a)) (Neg (deMorgan b))
deMorgan (Neg p) = Neg (deMorgan p)
deMorgan (Conj a b) = Conj (deMorgan a) (deMorgan b)
deMorgan (Disy a b) = Disy (deMorgan a) (deMorgan b)
deMorgan (Impl a b) = Impl (deMorgan a) (deMorgan b)
deMorgan (Syss a b) = Syss (deMorgan a) (deMorgan b)

-- Ejercicio 8: Regresa una fórmula proposicional equivalente al parámetro, quitando todas las ocurrencias de las implicaciones y doble implicaciones: p -> q = ¬p v q
equiv_op :: LProp -> LProp
equiv_op a@PTrue = a
equiv_op a@PFalse = a
equiv_op a@(Var n) = a
equiv_op (Neg p) = Neg (equiv_op p)
equiv_op (Conj a b) = Conj (equiv_op a) (equiv_op b)
equiv_op (Disy a b) = Disy (equiv_op a) (equiv_op b)
equiv_op (Impl a b) = Disy (Neg (equiv_op a)) (equiv_op b)
equiv_op (Syss a b) = equiv_op (Conj (Impl a b) (Impl b a))

-- Ejercicio 9: Aplica la propiedad de doble negación de forma exhaustiva en una fórmula proposicional.
dobleNeg :: LProp -> LProp
dobleNeg a@PTrue = a
dobleNeg a@PFalse = a
dobleNeg a@(Var n) = a
dobleNeg (Neg (Neg a)) = dobleNeg a
dobleNeg (Neg a) = Neg (dobleNeg a)
dobleNeg (Conj a b) = Conj (dobleNeg a) (dobleNeg b)
dobleNeg (Disy a b) = Disy (dobleNeg a) (dobleNeg b)
dobleNeg (Impl a b) = Impl (dobleNeg a) (dobleNeg b)
dobleNeg (Syss a b) = Syss (dobleNeg a) (dobleNeg b)

-- Ejercicio 10: Obtiene recursivamente el numero de conectivos de una fórmula proposicional.
num_conectivos :: LProp -> Int
num_conectivos (Neg a) = 1 + num_conectivos a
num_conectivos (Conj a b) = 1 + num_conectivos a + num_conectivos b
num_conectivos (Disy a b) = 1 + num_conectivos a + num_conectivos b
num_conectivos (Impl a b) = 1 + num_conectivos a + num_conectivos b
num_conectivos (Syss a b) = 1 + num_conectivos a + num_conectivos b
num_conectivos _ = 0

-- Ejercicio 11: Regresa la interpretación de una fórmula proposicional.
-- Dada una asignación ["p", T] regresa el valor de verdad con esa interpretación: interpretacion (p v q) [("p", 0), ("q", 1)] -> 1
-- interpretacion :: LProp -> Asignacion -> Int
-- interpretacion PTrue _ = 1
-- interpretacion PFalse _ = 0
-- interpretacion (Var n) a = if checaAsignaciones n a == 1 then 1 else 0
-- interpretacion (Neg p) a = if interpretacion p a == 1 then 0 else 1
-- interpretacion (Conj a b) e = if interpretacion a e == 1 && interpretacion b e == 1 then 1 else 0
-- interpretacion (Disy a b) e = if interpretacion a e == 1 || interpretacion b e == 1 then 1 else 0
-- interpretacion (Impl a b) e = if interpretacion a e == 1 && interpretacion b e == 0 then 0 else 1
-- interpretacion (Syss a b) e = if interpretacion a e == interpretacion b e then 1 else 0

-- checaAsignaciones :: String -> Asignacion -> Int
-- checaAsignaciones _ [] = error "Variable no asignada."
-- checaAsignaciones n ((s,int):xs) = if n == s then int else checaAsignaciones n xs
