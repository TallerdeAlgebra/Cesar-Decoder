import Data.Char


-- lista de minusculas de la "a" a la "z" para fasilitar otras tareas
mIn = "abcdefghijklmnopqrstuvwxyz"

-- "a" pertenece a una lista
pertenece :: Char -> [Char] -> Bool
pertenece a [] = False
pertenece a (b:bs) | a == b = True
                   | otherwise = pertenece a bs

esMin :: Char -> Bool				   
esMin a = pertenece a mIn


-- compara un elemento con una lista y le da un valor segun el primer momento en que aparece
valor :: Char -> [Char] -> Integer
valor a [] = 99999
-- el caso vasio no va a ser utilizado mas que para poder dar un caso bace a la recurrencia
valor a (b:bs) | a == b = 0
               | otherwise = 1+(valor a bs)

-- letANat no puede ser exaustiva por la definicion de tipos.
letANat :: Char -> Integer
letANat a | esMin a = valor a mIn


-- generalizacion de natALet para cualquier lista
toMin 0 (b:bs) = b
toMin n (b:bs) | 25 >= n = toMin (n-1) bs

natALet :: Integer -> Char 
natALet n | 25 >= n = toMin n mIn
          | otherwise = natALet (n-26) 

desplazar :: Integer -> Char -> Char
desplazar 0 a = a
desplazar n a | esMin a = natALet ((letANat a)+n)
              | otherwise = a
--

cantMinusc :: String -> Integer
cantMinusc [] = 0
cantMinusc (b:bs) | esMin b = 1+(cantMinusc bs)
                  | otherwise = cantMinusc bs