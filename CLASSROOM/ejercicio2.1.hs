-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Para ordenar una lista xs mediante el algoritmo de
-- ordenación rápida se selecciona el primer elemento x de xs, se divide
-- los restantes en los menores o iguales que x y en los mayores que x,
-- se ordena cada una de las dos partes y se unen los resultados. Por
-- ejemplo, para ordenar la lista [3,1,4,1,5,9,2] el proceso es el
-- siguiente:
-- or [3,1,4,1,5,9,2]
-- = or [1,1,2] ++ [3] ++ or [4,5,9]
-- = (or [1] ++ [1] ++ or [2]) ++ [3] ++ (or [] ++ [4] ++ or [5,9])
-- = ((or [] ++ [1] ++ or []) ++ [1] ++ (or [] ++ [2] ++ or []))
-- ++ [3] ++ ([] ++ [4] ++ (or [] ++ [5] ++ or [9]))
-- = (([] ++ [1] ++ []) ++ [1] ++ ([] ++ [2] ++ []))
-- ++ [3] ++ ([4] ++ ([] ++ [5] ++ (or [] ++ [9] ++ or [])))
-- = ([1] ++ [1] ++ [2] ++
-- ++ [3] ++ ([4] ++ ([5] ++ (or [] ++ [9] ++ or [])))
-- = ([1] ++ [1] ++ [2] ++
-- ++ [3] ++ ([4] ++ ([5] ++ ([] ++ [9] ++ [])))
-- = ([1] ++ [1] ++ [2] ++
-- ++ [3] ++ ([4] ++ ([5] ++ [9]))
-- = [1,1,2,3,4,5,9]
--
-- Definir la función
-- ordenaRapida :: Ord a => [a] -> [a]
-- tal que (ordenaRapida xs) es la lista obtenida ordenando por
-- selección la lista xs. Por ejemplo,
-- ordenaRapida [3,1,4,1,5,9,2] == [1,1,2,3,4,5,9]

ordenaRapida :: Ord a => [a] -> [a]
ordenaRapida [] = []
ordenaRapida (x:xs) = ordenaRapida menores ++ [x] ++ ordenaRapida mayores
  where
    menores = [y | y <- xs, y <= x]
    mayores = [y | y <- xs, y > x]
