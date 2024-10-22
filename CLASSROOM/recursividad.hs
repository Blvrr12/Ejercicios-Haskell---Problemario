---------------------------Recursividad -----------------------

-- Ejercicio 1. Definir por recursión la función
--potencia :: Integer -> Integer -> Integer
-- tal que (potencia x n) es x elevado al número natural n. Por ejemplo,
--potencia 2 3 == 8
potencia :: Integer -> Integer -> Integer
potencia _ 0 = 1
potencia x n = x * potencia x (n - 1)


-- Ejercicio 2. Dados dos números naturales, a y b, es posible
-- calcular su máximo común divisor mediante el Algoritmo de
-- Euclides. Este algoritmo se puede resumir en la siguiente fórmula:
--mcd(a,b) = a, si b = 0
--         = mcd (b, a módulo b), si b > 0
--
-- Definir la función
--mcd :: Integer -> Integer -> Integer
-- tal que (mcd a b) es el máximo común divisor de a y b calculado
-- mediante el algoritmo
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (a `mod` b)


-- ---------------------------------------------------------------------
-- Ejercicio 3, Definir por recursión la función
--pertenece :: Eq a => a -> [a] -> Bool
-- tal que (pertenece x xs) se verifica si x pertenece a la lista xs. Por
-- ejemplo,
--pertenece 3 [2,3,5] == True
--pertenece 4 [2,3,5] == False
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys)
  | x == y    = True
  | otherwise = pertenece x ys

-- ------------------------------------

-- Ejercicio 4. Definir por recursión la función
--tomar :: Int -> [a] -> [a]
-- tal que (tomar n xs) es la lista de los n primeros elementos de
-- xs. Por ejemplo,
--tomar 3 [4..12] => [4,5,6]
tomar :: Int -> [a] -> [a]
tomar 0 _      = []
tomar _ []     = []
tomar n (x:xs) = x : tomar (n - 1) xs

-- ---------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir, por comprensión, la función
--digitosC :: Integer -> [Integer]
-- tal que (digitosC n) es la lista de los dígitos del número n. Por
-- ejemplo,
--digitosC 320274 == [3,2,0,2,7,4]
digitosC :: Integer -> [Integer]
digitosC 0 = []
digitosC n = [read [c] | c <- show n]


-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir, por recursión, la función
-- sumaDigitosR :: Integer -> Integer
-- tal que (sumaDigitosR n) es la suma de los dígitos de n. Por ejemplo,
-- sumaDigitosR 3 == 3
-- sumaDigitosR 2454 == 15
-- sumaDigitosR 20045 == 11
-- -----------------------------
sumaDigitosR :: Integer -> Integer
sumaDigitosR 0 = 0
sumaDigitosR n = n `mod` 10 + sumaDigitosR (n `div` 10)
