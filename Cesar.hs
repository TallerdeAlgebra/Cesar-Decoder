import Data.Char


-- lista de minusculas de la "a" a la "z" para fasilitar otras tareas
mIn = ['a'..'z']

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
natALet  n | n <= 25 && n >= 0 = chr $ fromInteger n+97

desplazar :: Integer -> Char -> Char
desplazar n l | esMin l = natALet $ mod ((letANat l) + n) 26

cantMinusc :: String -> Integer
cantMinusc [] = 0
cantMinusc (b:bs) | esMin b = 1+(cantMinusc bs)
                  | otherwise = cantMinusc bs