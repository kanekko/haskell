{-
Ejemplo 1
-}
-- data Estudiante = Inscrito String String | Titulado String


{-
Ejemplo 2

module Fgeometricas where

data FiguraGeo = Circulo | Triangulo | Cuadrado

lados :: FiguraGeo -> Int 
lados Circulo = 1
lados Triangulo = 3
lados Cuadrado = 4
-}


{-
Ejemplo 3
-}
data Planeta = Mercurio | Venus | Tierra | Marte |
               Jupiter | Saturno | Urano | Neptuno

habitable :: Planeta -> Bool
habitable Tierra = True
habitable _      = False