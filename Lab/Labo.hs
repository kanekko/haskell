module Practica4 where

------------------------------------------------------------------------------------
-------------------------            PRACTICA 4            -------------------------
-------------------------            CIRCUITOS             -------------------------
------------------------------------------------------------------------------------

data Bit = Cero | Uno deriving Eq

instance Show Bit where
  show Cero = "0"
  show Uno = "1"
  
{-| Ejercicio 1: simula una compuerta lógica "and".
  Ejemplos:
  c_and Cero Cero
  c_and Cero Uno
  c_and Uno Cero
  c_and Uno Uno
-}
c_and :: Bit -> Bit -> Bit 
c_and Uno Uno = Uno
c_and _ _ = Cero

{-| Ejercicio 2: simula una compuerta lógica "or".
  Ejemplos:
  compuerta_or Cero Cero
  compuerta_or Cero Uno
  compuerta_or Uno Cero
  compuerta_or Uno Uno
-}
c_or :: Bit -> Bit -> Bit 
c_or Cero Cero = Cero
c_or _ _ = Uno

{-| Ejercicio 3: simula una compuerta lógica "not".
  Ejemplos:
  c_not Cero
  c_not Uno
-}
c_not :: Bit -> Bit  
c_not Cero = Uno
c_not _ = Cero

-- Ejercicio 4: simula una compuerta lógica "nand".
c_nand :: Bit -> Bit -> Bit 
c_nand Uno Uno = Cero
c_nand _ _ = Uno

-- Ejercicio 5: simula una compuerta lógica "nor".
c_nor :: Bit -> Bit -> Bit 
c_nor Cero Cero = Uno
c_nor _ _ = Cero

-- Ejercicio 6: simula una compuerta lógica "xor".
c_xor :: Bit -> Bit -> Bit 
c_xor a b = if a == b then Cero else Uno

-- Ejercicio 7: simula una compuerta lógica "xnor".
c_xnor :: Bit -> Bit -> Bit 
c_xnor a b = if a == b then Uno else Cero




-- Ejercicio 8: Función para medio sumador
half_add :: Bit -> Bit -> [Bit]
half_add x y = [c_xor x y, c_and x y]

-- Ejercicio 9: Función para sumador completo
full_add :: Bit -> Bit -> Bit -> [Bit]
full_add x y z = [c_xor x (c_xor y z), (c_or (c_and x y) (c_or (c_and x z) (c_and y z)))]



-- Ejercicio 10: Función de flip-flop RS
flip_flop_rs :: Bit -> Bit -> Bit -> Bit
flip_flop_rs q r s = c_or s (c_and (c_not r) q)

-- Ejercicio 11: Función para flip-flop D
flip_flop_d :: Bit -> Bit -> Bit
flip_flop_d q d = d 

-- Ejercicio 12: Función para flip-flop JK
flip_flop_jk :: Bit -> Bit -> Bit -> Bit
flip_flop_jk q j k = c_or (c_and j (c_not q)) (c_and (c_not j) q)

-- Ejercicio 13: Función para flip-flop T
flip_flop_t :: Bit -> Bit -> Bit
flip_flop_t q t = c_xor q t
