module Practica3 where

{-----------------------------------------------------------------------------------
-------------------------            PRACTICA 3            -------------------------
---------------------           LÓGICA PROPOSICIONAL        ------------------------
-----------------------------------------------------------------------------------}

{-|
  Tipo de dato LProp para lógica proposicional
  Basado en gramática de fórmulas proposicionales
-}
data LProp = PTrue | PFalse | Var Nombre | Neg LProp | Conj LProp LProp | Disy LProp LProp | Impl LProp LProp | Syss LProp LProp deriving Eq

type Nombre = String
type Asignacion = [(String, Int)]

{-|
  Ejercicio 1: Visualización del tipo de dato LProp.
  Las variables se definen como: 
  Var "p"
  Neg (Var “p”)
  Conj (Neg (Var "p")) (Var "p")
  Disy (Var "p") (Var "q")
  Impl (Var "p") (Var "q")
  Syss (Var "p") (Var "q")
-}
instance Show LProp where 
  show PTrue = "True"                                       -- T
  show PFalse = "False"                                     -- F
  show (Var x) = x                                          -- P  
  show (Neg p) = "¬" ++ show p                              -- ¬(P)
  show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")" -- (P ∧ Q)
  show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")" -- (P ∨ Q)
  show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")" -- (P → Q)
  show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")" -- (P ↔ Q)


{-| PRACTICA
Ejercicio 2: Regresa las variables de una fórmula proposicional.
Define recursivamente la variable en una fórmula.
  Ejemplos:
  vars (Syss (Impl (Var "a") (Var "b")) (Impl (Var "c") (Conj (Var "b") (Var "c"))))
  vars (Impl PTrue (Var "x"))
  vars (Impl PTrue PFalse)
  vars (Conj (Disy (Var "z") (Var "w")) PFalse)
  vars (Neg (Var "t"))
  vars (Neg (Neg PTrue))
-}
vars :: LProp ->[Nombre]
vars PTrue = []
vars PFalse = []
vars (Var n) = [n]
vars (Neg p) = vars p
vars (Conj p q) = elimina_repetidos (vars p ++ vars q)
vars (Disy p q) = elimina_repetidos (vars p ++ vars q)
vars (Impl p q) = elimina_repetidos (vars p ++ vars q)
vars (Syss p q) = elimina_repetidos (vars p ++ vars q)

-- Auxiliar: Elimina variables repetidas
elimina_repetidos :: (Eq a) => [a] -> [a]
elimina_repetidos [] = []
elimina_repetidos (x:xs) = if elem x xs then elimina_repetidos xs else x:(elimina_repetidos xs)


{-|
  Ejercicio 3: Asocia a la derecha en una fórmula proposicional.
  Ejemplos:
  asocia_der (Conj (Conj (Var "a") (Var "b")) (Var "c"))
  asocia_der (Conj (Var "a") (Conj (Var "b") (Var "c")))
  asocia_der (Disy (Disy (Var "p") (Var "q")) (Var "r"))
  asocia_der (Disy (Var "p") (Disy (Var "q") (Var "r")))
  asocia_der (Syss PFalse PFalse)
-}
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


{-|
  Ejercicio 4: Asocia a la izquierda en una fórmula proposicional. 
  Ejmeplos:
  ((p v q) v p) y obtiene output (p v (q v p)) 
  asocia_izq (Conj (Conj (Var "a") (Var "b")) (Var "c"))
  asocia_izq (Conj (Var "a") (Conj (Var "b") (Var "c")))
  asocia_izq (Disy (Disy (Var "p") (Var "q")) (Var "r"))
  asocia_izq (Disy (Var "p") (Disy (Var "q") (Var "r")))
  asocia_izq (Syss PTrue PTrue)
-}
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


{-|
  Ejercicio 5: Aplica la propiedad de conmutatividad de forma exhaustiva en una fórmula proposicional.
  Ejemplos:
  conm (Conj (Neg (Var "u")) PTrue)
  conm (Disy (Var "s") (Var "w"))
  conm (Syss (Var "u") (Conj PTrue (Var "u")))
  conm (Disy (Conj (Var "a") (Var "b")) (Var "c"))
  conm (Neg (Disy (Var "a") (Impl (Var "b") (Var "a"))))
-}
conm :: LProp -> LProp
conm a@PTrue = a
conm a@PFalse = a
conm a@(Var n) = a
conm (Neg p) = Neg (conm p)
conm (Conj a b) = Conj (conm b) (conm a)
conm (Disy a b) = Disy (conm b) (conm a)
conm (Impl a b) = Impl (conm a) (conm b)
conm (Syss a b) = Syss (conm a) (conm b)


{-|
  Ejercicio 6: Conmuta las variables en una fórmula proposicional. 
  Por ejemplo: p v q = q v p.
  dist (Conj (Neg (Var "d")) (Disy (Var "e") (Var "f")))
  dist (Disy (Var "s") (Conj (Var "t") (Var "u")))
  dist (Conj (Var "a") (Impl (Var "b") (Var "a")))
  dist (Neg (Var "r"))
  dist (Disy (Var "i") PFalse)
  dist PTrue
-}
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


{-|
  Ejercicio: Identidad
  Ejemplos:
  ident (Conj (Var "a") (Var "b"))
  ident (Conj (Var "a") PTrue)
  ident (Impl (Var "a") (Var "b"))
  ident (Impl (Var "a") PTrue)
-}
ident :: LProp -> LProp
ident a@PTrue = a
ident a@PFalse = a
ident a@(Var n) = a
ident (Neg p) = Neg (ident p)
ident (Conj a PTrue) = ident a
ident (Conj a b) = Conj (ident a) (ident b)
ident (Disy a PFalse) = ident a
ident (Disy a b) = Disy (ident a) (ident b)
ident (Impl a b) = Impl (ident a) (ident b)
ident (Syss a b) = Syss (ident a) (ident b)


{-
  Ejercicio: Elemento nulo.
  Ejemplos:
  nulo (Conj (Var "a") PTrue)
  nulo (Conj (Var "a") PFalse)
  nulo (Disy (Var "a") PTrue)
  nulo (Disy (Var "a") PFalse)
  nulo (Impl (Var "a") (Var "b"))
-}
nulo :: LProp -> LProp
nulo a@PTrue = a
nulo a@PFalse = a
nulo a@(Var n) = a
nulo (Neg p) = Neg (nulo p)
nulo (Conj a PFalse) = PFalse
nulo (Conj a b) = Conj (nulo a) (nulo b)
nulo (Disy a PTrue) = PTrue
nulo (Disy a b) = Disy (nulo a) (nulo b)
nulo (Impl a b) = Impl (nulo a) (nulo b)
nulo (Syss a b) = Syss (nulo a) (nulo b)


{-
  Ejercicio: Idempotencia.
  Ejemplos:
  idem (Conj (Var "a") (Var "b"))
  idem (Conj (Var "a") (Var "a"))
  idem (Disy (Var "a") (Var "b"))
  idem (Disy (Var "a") (Var "a"))
-}
idem :: LProp -> LProp
idem a@PTrue = a
idem a@PFalse = a
idem a@(Var n) = a
idem (Neg p) = Neg (idem p)
idem (Conj a b) = if a == b then idem a else Conj (idem a) (idem b)
idem (Disy a b) = if a == b then idem a else Disy (idem a) (idem b)
idem (Impl a b) = Impl (idem a) (idem b)
idem (Syss a b) = Syss (idem a) (idem b)


{-| PRACTICA
  Ejercicio 7: Aplica las propiedades de DeMorgan en una fórmula proposicional.
  Ejemplos:
  deMorgan (Neg (Conj (Var "a") (Var "b")))
  deMorgan (Neg (Disy (Var "c") (Var "d")))
  deMorgan (Syss (Impl (Var "p") PFalse) (Var "s"))
  deMorgan (Neg (Neg (Var "o")))
  deMorgan (Neg (Impl (Var "h") (Var "j")))
-}
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


{-
  Ejercicio: Contradicción.
  Ejemplos:
  contra (Conj (Var "p") (Var "p"))
  contra (Conj (Var "p") (Neg(Var "p")))
-}
contra :: LProp -> LProp
contra a@PTrue = a
contra a@PFalse = a
contra a@(Var n) = a
contra (Neg a) = Neg (contra a)
contra (Conj a (Neg b)) = if a == b then contra PFalse else Conj (contra a) (contra b)
contra (Conj a b) = Conj (contra a) (contra b)
contra (Disy a b) = Disy (contra a) (contra b)
contra (Impl a b) = Impl (contra a) (contra b)
contra (Syss a b) = Syss (contra a) (contra b)


{-
  Ejercicio: Tercero excluido
  Ejemplos:
  tercero (Disy (Var "a") (Neg(Var "a")))
  tercero (Disy (Var "a") (Var "a"))
  tercero (Disy (Var "a") (Var "b"))
  tercero (Disy (Var "a") (Neg(Var "b")))
-}
tercero :: LProp -> LProp
tercero a@PTrue = a
tercero a@PFalse = a
tercero a@(Var n) = a
tercero (Neg a) = Neg (tercero a)
tercero (Conj a b) = Conj (tercero a) (tercero b)
tercero (Disy a (Neg b)) = if a == b then tercero PTrue else Disy (tercero a) (tercero b)
tercero (Disy a b) = Disy (tercero a) (tercero b)
tercero (Impl a b) = Impl (tercero a) (tercero b)
tercero (Syss a b) = Syss (tercero a) (tercero b)


{-| PRACTICA
  Ejercicio 8: Regresa una fórmula proposicional equivalente al parámetro, 
               quitando todas las ocurrencias de las implicaciones y doble implicaciones: p -> q = ¬p v q
  Ejemplos:
  equiv_op (Impl PFalse (Var "q"))
  equiv_op (Syss PFalse PTrue)
  equiv_op (Syss (Var "k") (Neg PFalse))
  equiv_op (Disy (Var "c") (Conj (Var "u") (Var "b")))
  equiv_op (Conj (Disy (Var "s") (Var "w")) (Impl (Var "e") (Var "d")))
-}
equiv_op :: LProp -> LProp
equiv_op a@PTrue = a
equiv_op a@PFalse = a
equiv_op a@(Var n) = a
equiv_op (Neg p) = Neg (equiv_op p)
equiv_op (Conj a b) = Conj (equiv_op a) (equiv_op b)
equiv_op (Disy a b) = Disy (equiv_op a) (equiv_op b)
equiv_op (Impl a b) = Disy (Neg (equiv_op a)) (equiv_op b)
equiv_op (Syss a b) = equiv_op (Conj (Impl a b) (Impl b a))


{-| PRACTICA
  Ejercicio 9: Aplica la propiedad de doble negación de forma exhaustiva en una fórmula proposicional.
  Ejemplos:
  dobleNeg (Neg (Neg (Conj (Neg (Neg (Var "x"))) (Var "y"))))
  dobleNeg (Neg (Conj (Neg (Neg (Var "x"))) (Var "y")))
  dobleNeg (Disy (Neg (Neg (Var "z"))) (Conj (Var "q") (Var "a")))
  dobleNeg (Neg (Neg (Var "P")))
  dobleNeg (Neg (Neg (Neg (Neg (Neg (Var "s"))))))
-}
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


{-| PRACTICA
  Ejercicio 10: Obtiene recursivamente el numero de conectivos de una fórmula proposicional.
  Ejemplos:
  num_conectivos (Var "p")
  num_conectivos (Disy (Var "u") (Disy (Var "w") (Var"a")))
  num_conectivos (Syss (Var "n") (Neg (Conj PTrue (Var "m"))))
  num_conectivos (Syss (Impl (Var "e") (Var "k")) (Impl (Var "j") (Conj (Var "a") (Var "l"))))
  num_conectivos (Neg (Neg (Neg (Neg (Neg (Var "s"))))))
-}
num_conectivos :: LProp -> Int
num_conectivos (Neg a) = 1 + num_conectivos a
num_conectivos (Conj a b) = 1 + num_conectivos a + num_conectivos b
num_conectivos (Disy a b) = 1 + num_conectivos a + num_conectivos b
num_conectivos (Impl a b) = 1 + num_conectivos a + num_conectivos b
num_conectivos (Syss a b) = 1 + num_conectivos a + num_conectivos b
num_conectivos _ = 0


{-| PRACTICA
  Ejercicio 11: Obtiene recursivamente el numero de variables de una fórmula proposicional.
  Ejemplos:
  num_variables (Var "p")
  num_variables (Disy (Var "u") (Disy (Var "w") (Var"a")))
  num_variables (Syss (Var "n") (Neg (Conj PTrue (Var "m"))))
  num_variables (Syss (Impl (Var "e") (Var "k")) (Impl (Var "j") (Conj (Var "a") (Var "l"))))
  num_variables (Neg (Neg (Neg (Neg (Neg (Var "s"))))))  
-}
num_variables :: LProp -> Int
num_variables (Neg a) = num_variables a
num_variables (Conj a b) = num_variables a + num_variables b
num_variables (Disy a b) = num_variables a + num_variables b
num_variables (Impl a b) = num_variables a + num_variables b
num_variables (Syss a b) = num_variables a + num_variables b
num_variables _ = 1


{-| PRACTICA
  Ejercicio 12: Obtiene recursivamente la profundidad de una fórmula proposicional.
  Ejemplos:
  profundidad (Var "p")
  profundidad (Disy (Var "u") (Disy (Var "w") (Var"a")))
  profundidad (Syss (Var "n") (Neg (Conj PTrue (Var "m"))))
  profundidad (Syss (Impl (Var "e") (Var "k")) (Impl (Var "j") (Conj (Var "a") (Var "l"))))
  profundidad (Neg (Neg (Neg (Neg (Neg (Var "s"))))))  
-}
profundidad :: LProp -> Int
profundidad (Neg a) = 1 + profundidad a
profundidad (Conj a b) = 1 + max (profundidad a) (profundidad b)
profundidad (Disy a b) = 1 + max (profundidad a) (profundidad b)
profundidad (Impl a b) = 1 + max (profundidad a) (profundidad b)
profundidad (Syss a b) = 1 + max (profundidad a) (profundidad b)
profundidad _ = 0


{-| PRACTICA
  Ejercicio 13: Regresa la interpretación de una fórmula proposicional.
  Dada una asignación ["p", T] regresa el valor de verdad con esa interpretación: interpretacion (p v q) [("p", 0), ("q", 1)] -> 1
  Ejemplos:
  interpretacion (Disy (Var "p") (Var "q")) [("p", 0), ("q", 1)]
  interpretacion (Conj (Var "p") (Var "q")) [("p", 0), ("q", 1)]
  interpretacion (Conj (Var "p") (Var "q")) [("p", 1), ("q", 1)]
  interpretacion (Disy (Impl (Var "p") (Var "q")) (Var "r")) [("p", 1), ("q", 0), ("r", 1)]
-}
interpretacion :: LProp -> Asignacion -> Int
interpretacion PTrue _ = 1
interpretacion PFalse _ = 0
interpretacion (Var n) a = if checaAsignaciones n a == 1 then 1 else 0
interpretacion (Neg p) a = if interpretacion p a == 1 then 0 else 1
interpretacion (Conj a b) e = if interpretacion a e == 1 && interpretacion b e == 1 then 1 else 0
-- interpretacion (Disy a b) e = if interpretacion a e == 1 || interpretacion b e == 1 then 1 else 0
interpretacion (Disy a b) e = if interpretacion a e == 0 && interpretacion b e == 0 then 0 else 1
interpretacion (Impl a b) e = if interpretacion a e == 1 && interpretacion b e == 0 then 0 else 1
interpretacion (Syss a b) e = if interpretacion a e == interpretacion b e then 1 else 0

checaAsignaciones :: String -> Asignacion -> Int
checaAsignaciones _ [] = error "Variable no asignada."
checaAsignaciones n ((s,int):xs) = if n == s then int else checaAsignaciones n xs
