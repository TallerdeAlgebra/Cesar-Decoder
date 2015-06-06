----------------------------------------------
--											--
--              T.P. Álgebra                --
--											--
--											--
-- Iglesias, Manuel							--
-- Ruiz, Leandro E.							--
-- Desconocido, Unknown						--
--											--
----------------------------------------------

import Data.Char


-- Lista de minusculas de la "a" a la "z" para
-- facilitar otras tareas
min :: [Char]
min = ['a'..'z']  -- Vos tomá lo que te parezca

-- lista de minusculas de la "a" a la "z" para fasilitar otras tareas
mIn = "abcdefghijklmnopqrstuvwxyz"

-- "a" pertenece a una lista
pertenece :: Char -> [Char] -> Bool
pertenece a [] = False
pertenece a (b:bs) = a == b || pertenece a bs

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

-- Te propongo esta solución mejor (te ahorras recursividad)
letANat :: Char -> Integer
letANat a | esMin a = (ord a)-97  -- Valor de ASCII de la letra

-- generalizacion de natALet para cualquier lista
toMin 0 (b:bs) = b
toMin n (b:bs) | 25 >= n = toMin (n-1) bs
 
natALet n | 25 >= n = toMin n mIn
          | otherwise = natALet (n-26) 

-- Si elegís mi otra implementación, quedaría así
natALet :: Integer -> Char
natALet  n | n <= 27 && n >= 0 = chr a

desplazar 0 a = a
desplazar n a | esMin a = natALet ((letANat a)+n)
              | otherwise = a

-- Pensando en que "desplazar" es ciclico
-- podés usar mod 27 para el desplazar
-- Fijate si te convence
desplazar n l | esMin l = natAlet (mod 27 (letANat l + n))