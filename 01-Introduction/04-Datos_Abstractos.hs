module Fgeometricas where

data FiguraGeo = Circulo | Triangulo | Cuadrado

lados :: FiguraGeo -> Int 
lados Circulo = 1
lados Triangulo = 3
lados Cuadrado = 4