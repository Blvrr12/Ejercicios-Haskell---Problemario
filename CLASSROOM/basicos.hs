-- Ejercicio 1. Definir la función promedio3 tal que (promedio3 x y z) es
-- la promedio aritmética de los números x, y y z. Por ejemplo,
-- promedio3 1 3 8 == 4.0
-- promedio3 (-1) 0 7 == 2.0
-- promedio3 (-3) 0 3 == 0.0

promedio3 :: Float -> Float -> Float -> Float
promedio3 x y z = (x + y + z) / 3


-- Ejercicio 2. Definir la función sumaMonedas tal que
-- (sumaMonedas a b c d e) es la suma de las monedas  correspondientes a
-- a monedas de 1 peso, b de 2 pesos, c de 5 pesos, d 10 pesos y
-- e de 20 pesos. Por ejemplo,
--sumaMonedas 0 0 0 0 1 == 20
--sumaMonedas 0 0 8 0 3 == 100
--sumaMonedas 1 1 1 1 1 == 38
-- -----------------------------------

sumaMonedas :: Num a => a -> a -> a -> a -> a -> a
sumaMonedas a b c d e = a * 1 + b * 2 + c * 5 + d * 10 + e * 20


-- Ejercicio 3. Definir la función volumenEsfera tal que
-- (volumenEsfera r) es el volumen de la esfera de radio r. Por ejemplo,
-- volumenEsfera 10 == 4188.790204786391
-- Indicación: Usar la constante pi.
-- -----------------------------------

volumenEsfera :: Float -> Float
volumenEsfera r = (4 / 3) * pi * r^3

-- Ejercicio 4. Definir la función areaDeCoronaCircular tal que
-- (areaDeCoronaCircular r1 r2) es el área de una corona circular de
-- radio interior r1 y radio exterior r2. Por ejemplo,
--areaDeCoronaCircular 1 2 == 9.42477796076938
--areaDeCoronaCircular 2 5 == 65.97344572538566
--areaDeCoronaCircular 3 5 == 50.26548245743669

areaDeCoronaCircular :: Float -> Float -> Float
areaDeCoronaCircular r1 r2 = pi * (r2^2 - r1^2)


-- Ejercicio 5. Definir la función ultimaCifra tal que (ultimaCifra x)
-- es la última cifra del número x. Por ejemplo,
-- ultimaCifra 325 == 5
-- Indicación: Usar la función rem

ultimaCifra :: Int -> Int
ultimaCifra x = x `rem` 10 

-- Ejercicio 6. Definir la función maxTres tal que (maxTres x y z) es
-- el máximo de x, y y z. Por ejemplo,
-- maxTres 6 2 4 == 6
-- maxTres 6 7 4 == 7
-- maxTres 6 7 9 == 9
-- Indicación: Usar la función max.

maxTres :: Ord a => a -> a -> a -> a
maxTres x y z = max x (max y z)

-- Ejercicio 7. Definir la función rota1 tal que (rota1 xs) es la lista
-- obtenida poniendo el primer elemento de xs al final de la lista. Por
-- ejemplo,
-- rota1 [3,2,5,7] == [2,5,7,3]
-- Indicación: Usar la función tail head
-- -----------------------------------
rota1 :: [a] -> [a]
rota1 xs = tail xs ++ [head xs]

-- Ejercicio 8. Definir la función rota tal que (rota n xs) es la lista
-- obtenida poniendo los n primeros elementos de xs al final de la
-- lista. Por ejemplo,
-- rota 1 [3,2,5,7] == [2,5,7,3]
-- rota 2 [3,2,5,7] == [5,7,3,2]
-- rota 3 [3,2,5,7] == [7,3,2,5]
-- Indicación: Usar la función drop take

rota :: Int -> [a] -> [a]
rota n xs = drop n xs ++ take n xs

-- Ejercicio 9. Definir la función rango tal que (rango xs) es la
-- lista formada por el menor y mayor elemento de xs.
-- rango [3,2,7,5] == [2,7]
-- Indicación: Usar minimum y maximum

rango :: Ord a => [a] -> [a]
rango xs = [minimum xs, maximum xs]

-- Ejercicio 10. Definir la función palindromo tal que (palindromo xs) se
-- verifica si xs es un palíndromo; es decir, es lo mismo leer xs de
-- izquierda a derecha que de derecha a izquierda. Por ejemplo,
-- palindromo [3,2,5,2,3] == True
-- palindromo [3,2,5,6,2,3] == False
-- Indicación: utiliza la función reverse

palindromo :: Eq a => [a] -> Bool
palindromo xs = xs == reverse xs

-- Ejercicio 11. Definir la función interior tal que (interior xs) es la
-- lista obtenida eliminando los extremos de la lista xs. Por ejemplo,
-- interior [2,5,3,7,3] == [5,3,7]
-- interior [2..7] == [3,4,5,6]
-- Indicación : utiliza la función tail init

interior :: [a] -> [a]
interior xs = tail (init xs)

-- Ejercicio 12. Definir la función segmento tal que (segmento m n xs) es
-- la lista de los elementos de xs comprendidos entre las posiciones m y
-- n. Por ejemplo,
-- segmento 3 4 [3,4,1,2,7,9,0] == [1,2]
-- segmento 3 5 [3,4,1,2,7,9,0] == [1,2,7]
-- segmento 5 3 [3,4,1,2,7,9,0] == []
-- Indicación: utiliza la función drop take

segmento :: Int -> Int -> [a] -> [a]
segmento m n (xs) =  drop (m - 1)  (take n xs)

-- Ejercicio 13. Definir la función extremos tal que (extremos n xs) es
-- la lista formada por los n primeros elementos de xs y los n finales
-- elementos de xs. Por ejemplo,
-- extremos 3 [2,6,7,1,2,4,5,8,9,2,3] == [2,6,7,9,2,3]
-- Indicación: utiliza la función take drop

extremos :: Int -> [a] -> [a]
extremos n xs = take n xs ++ drop (length xs - n) xs

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función mediano tal que (mediano x y z) es el
-- número mediano de los tres números x, y y z. Por ejemplo,
-- mediano 3 2 5 == 3
-- mediano 2 4 5 == 4
-- mediano 2 6 5 == 5
-- mediano 2 6 6 == 6
-- Indicación: Usar maximum y minimum.

mediano :: (Ord a, Num a) => a -> a -> a -> a
mediano x y z = x + y + z - minimum [x, y, z] - maximum [x, y, z]

-- Ejercicio 15. Definir la función tresIguales tal que
-- (tresIguales x y z) se verifica si los elementos x, y y z son
-- iguales. Por ejemplo,
-- tresIguales 4 4 4 == True
-- tresIguales 4 3 4 == False

tresIguales :: Eq a => a -> a -> a -> Bool
tresIguales x y z = x == y && y == z


-- Ejercicio 16. Definir la función tresDiferentes tal que
-- (tresDiferentes x y z) se verifica si los elementos x, y y z son
-- distintos. Por ejemplo,
-- tresDiferentes 3 5 2 == True
-- tresDiferentes 3 5 3 == False

tresDiferentes :: Eq a => a -> a -> a -> Bool
tresDiferentes x y z = x /= y && y /= z && x /= z

-- Ejercicio 17. Definir la función cuatroIguales tal que
-- (cuatroIguales x y z u) se verifica si los elementos x, y, z y u son
-- iguales. Por ejemplo,
-- cuatroIguales 5 5 5 5 == True
-- cuatroIguales 5 5 4 5 == False
-- Indicación: Usar la función tresIguales.

cuatroIguales :: Eq a => a -> a -> a -> a -> Bool
cuatroIguales x y z u = tresIguales x y z && z == u

