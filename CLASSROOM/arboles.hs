--____________________________Árboles___________________________
--Crea un nuevo tipo de dato árbol
--Funciones
--1.- Insertar desde un arreglo 
--2.- Buscar un elemento en un árbol
--3.- Recorridos (Inorden, posorden, preorden)

-- Definimos el tipo de dato arbol
data Arbol a = Hoja | Nodo a (Arbol a) (Arbol a) deriving (Show, Eq)

-- Función para insertar un elemento en un árbol binario de búsqueda
insertar :: (Ord a) => a -> Arbol a -> Arbol a
insertar x Hoja = Nodo x Hoja Hoja
insertar x (Nodo valor izq der)
  | x < valor = Nodo valor (insertar x izq) der
  | x > valor = Nodo valor izq (insertar x der)
  | otherwise = Nodo valor izq der -- si el numero se inserta y ya hay uno, se mantiene o no se introduce

-- Función para insertar una lista de elementos en el árbol
insertarArreglo :: (Ord a) => [a] -> Arbol a
insertarArreglo = foldr insertar Hoja

-- Función para buscar un elemento en un árbol binario de búsqueda
buscar :: (Ord a) => a -> Arbol a -> Bool
buscar _ Hoja = False
buscar x (Nodo valor izq der)
  | x == valor = True
  | x < valor = buscar x izq
  | x > valor = buscar x der

  -- Recorrido inorden (izquierda, raiz, derecha)
inorden :: Arbol a -> [a]
inorden Hoja = []
inorden (Nodo valor izq der) = inorden izq ++ [valor] ++ inorden der

-- Recorrido preorden (raiz, izquierda, derecha)
preorden :: Arbol a -> [a]
preorden Hoja = []
preorden (Nodo valor izq der) = [valor] ++ preorden izq ++ preorden der

-- Recorrido posorden (izquierda, derecha, Raiz)
posorden :: Arbol a -> [a]
posorden Hoja = []
posorden (Nodo valor izq der) = posorden izq ++ posorden der ++ [valor]



