{-
Ejemplo1
-}
type Nombre = String 
type Carrera = String

data Estudiante = Inscrito Nombre Carrera | Titulado Nombre

{-
Ejemplo 2
-}
type NombreApellido  = String
type Edad    = Int
type Salario = Float

data Trabajador = Alta NombreApellido Edad Salario | Baja NombreApellido