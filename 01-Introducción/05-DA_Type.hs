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

data Trabajador = Interno NombreApellido Edad Salario | Externo NombreApellido

{-
Ejemplo 3
-}
type NombreCompleto  = String
type SalarioNuevo = Float
type Empresa = String

data Empleado = EmpleadoInterno NombreCompleto SalarioNuevo | EmpleadoExterno NombreCompleto Empresa

showEmpleado :: Empleado -> String
showEmpleado (EmpleadoInterno n s) = n ++ " gana $" ++ (show s)
showEmpleado (EmpleadoExterno n e) = n ++ " es de la empresa " ++ e


{-
Ejemplo 4
-}
data AExp = Lit Int | Add AExp AExp | Mul AExp AExp

eval :: AExp -> Int
eval (Lit x) = x

evalrec :: AExp -> Int
evalrec (Lit x)   = eval (Lit x)
evalrec (Add x y) = (evalrec x) + (evalrec y)
evalrec (Mul x y) = (evalrec x) * (evalrec y)