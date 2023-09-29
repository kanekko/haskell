module Tverdad where

data Prop = T | F | V NVar | Not Prop | And Prop Prop | Or Prop Prop | Imp Prop Prop | Eq Prop Prop deriving (Eq,Show)
data NVar = P Int deriving (Eq,Show)

type Renglon = ([Bool],Bool)
type Tabla = [Renglon]

-- 0.
-- instance Show Prop where
--     show T = "True"
--     show F = "False"
--     show (V n) = NVar n
    -- show (Neg p) = "¬" ++ show p
    -- show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
    -- show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
    -- show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
    -- show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

-- 1 vars
-- Cuenta las variables de una proposición.
vars :: Prop -> Int
vars T = 0
vars F = 0
vars (V n) = 1
vars (Not p) = vars p
vars (And p q) = (vars p) + (vars q)
vars (Or p q) = (vars p) + (vars q)
vars (Imp p q) = (vars p) + (vars q)
vars (Eq p q) = (vars p) + (vars q)



