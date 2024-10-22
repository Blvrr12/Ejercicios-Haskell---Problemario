import Data.List (sortBy,maximumBy, minimumBy)

------------------------------Nuevos tipos de datos------------
--1.- Crea un nuevo tipo Estudiate con los siguientes atributos
-- Nombre, Apellido, Edad, Número de control 
-- Genera una lista de un mínimo de 10 estudiantes en donde obtendras

-- Lista ordenada de los estudiantes de acuedo a la edad
-- Obtener al estudiante menor, mayor
-- Obtener el promedio de edades.

-- Definimos el tipo de dato 
data Estudiante = Estudiante {
    nombre :: String,
    apellido :: String,
    edad :: Int,
    numControl :: Int
} deriving (Show, Eq)

estudiantes :: [Estudiante]
estudiantes = [
    Estudiante "Kai" "Hakala" 20 12345,
    Estudiante "Aria" "Bianchi" 19 12346,
    Estudiante "Ravi" "Patel" 22 12347,
    Estudiante "Saskia" "Keller" 18 12348,
    Estudiante "Mateo" "Santana" 21 12349,
    Estudiante "Elif" "Demir" 20 12350,
    Estudiante "Igor" "Romanov" 23 12351,
    Estudiante "Anika" "Vasileva" 19 12352,
    Estudiante "Omar" "Almeida" 21 12353,
    Estudiante "Yara" "Nakamura" 22 12354
  ]

-- Función para ordenar la lista de estudiantes por edad
ordenaPorEdad :: [Estudiante] -> [Estudiante]
ordenaPorEdad = sortBy (\e1 e2 -> compare (edad e1) (edad e2))

-- Función para obtener al estudiante más joven
estudianteMasJoven :: [Estudiante] -> Estudiante
estudianteMasJoven = minimumBy (\e1 e2 -> compare (edad e1) (edad e2))


-- Función para obtener al estudiante más viejo
estudianteMasViejo :: [Estudiante] -> Estudiante
estudianteMasViejo = maximumBy (\e1 e2 -> compare (edad e1) (edad e2))

-- Función para calcular el promedio de edades
promedioEdad :: [Estudiante] -> Float
promedioEdad es = fromIntegral (sum (map edad es)) / fromIntegral (length es)

