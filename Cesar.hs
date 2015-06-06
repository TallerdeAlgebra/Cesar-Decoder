import Data.Char 
import Data.String


-- lista de minusculas de la "a" a la "z" para fasilitar otras tareas
mIn = ['a'..'z']
tdA = "taller de algebra"

-- "a" pertenece a una lista
pertenece :: Char -> [Char] -> Bool
pertenece a [] = False
pertenece a (b:bs) = a == b || pertenece a bs

esMin :: Char -> Bool				   
esMin a = pertenece a mIn

-- letANat no puede ser exaustiva por la definicion de tipos.
letANat :: Char -> Integer
letANat a | esMin a = toInteger $ (ord  a)-97  -- Valor de ASCII de la letra

-- generalizacion de natALet para cualquier lista
natALet :: Integer -> Char
natALet  n = chr $ fromInteger (mod n 26)+97

desplazar :: Integer -> Char -> Char
desplazar n l | esMin l = natALet $ mod ((letANat l) + n) 26

cantMinusc :: String -> Integer
cantMinusc [] = 0
cantMinusc (b:bs) | esMin b = 1+(cantMinusc bs)
                  | otherwise = cantMinusc bs
--

contar :: Char -> String -> Integer
contar a [] = 0
contar a (b:bs) | a==b = 1+contar a bs
                | otherwise = contar a bs
--
listar [] = []
listar (b:bs)= b:(listar bs)

codificar :: Integer -> String -> String
codificar n [] = []
codificar n (b:bs) | esMin b = (desplazar n b): codificar n bs
                   | otherwise = b : codificar n bs
--

decodificar :: Integer -> String -> String
decodificar n [] = []
decodificar n (b:bs) | esMin b = (desplazar (-n) b): codificar (-n) bs
                     | otherwise = b : codificar (-n) bs