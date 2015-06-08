import Data.Char 
import Data.String

------------------------------------------ITEM 1----------------------------------------------

-- lista de minusculas de la "a" a la "z" para fasilitar otras tareas
mIn = ['a'..'z']

-- String para probar funciones
tdA = "taller de algebra"



-- "a" pertenece a una lista [EDIT by santi, quisiera probar esta función, creo que tiene algo mal]
pertenece :: Char -> [Char] -> Bool
pertenece a [] = False
pertenece a (b:bs) = a == b || pertenece a bs

-- invoca la lista de minúsculas mIn, es verdadero por definición
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

------------------------------------------ITEM 2----------------------------------------------

codificar :: Integer -> String -> String
codificar n [] = []
codificar n (b:bs) | esMin b = (desplazar n b): codificar n bs
                   | otherwise = b : codificar n bs

------------------------------------------ITEM 3----------------------------------------------

decodificar :: Integer -> String -> String
decodificar n [] = []
decodificar n (b:bs) | esMin b = (desplazar (-n) b): codificar (-n) bs
                     | otherwise = b : codificar (-n) bs

------------------------------------------ITEM 4----------------------------------------------

-- generalizacion de la funcion frec
frecLEnL [] l = []
frecLEnL (b:bs) l = ((fromInteger $(contar b l))/(fromInteger $(cantMinusc l))) : frecLEnL bs l

frec :: String -> [Float]
frec l = frecLEnL mIn l

------------------------------------------ITEM 5----------------------------------------------

rotar :: Integer -> String -> String
rotar n [] = []
rotar n (b:bs) = drop (fromInteger $ n) (b:bs) ++ take (fromInteger $ n) (b:bs)

------------------------------------------ITEM 6----------------------------------------------
chi2 :: [Float] -> [Float] -> Float
chi2 _ [] = 0
chi2 (b:bs) (c:cs) = (((b-c)^2)/c) + (chi2 (bs) (cs))

  

------------------------------------------ITEM 7----------------------------------------------
-- Frecuencia del abecedario en el idioma español
esp :: [Float]
esp = [12.52, 1.42, 4.67, 5.85, 13.67, 0.67, 1.01, 0.70, 6.24, 0.44, 0.01, 4.96, 3.15, 6.70, 8.67, 2.51, 0.88, 6.86, 7.97, 4.62, 3.92, 0.90, 0.02, 0.22, 0.90, 0.52]

--distancia chi2 de la frec de apariciopn con las 25 rotaciones de la lista esp y la lista esp
rotarFloat :: Integer -> [Float] -> [Float]
rotarFloat n [] = []
rotarFloat n (b:bs) = drop (fromInteger $ n) (b:bs) ++ take (fromInteger $ n) (b:bs)

chi25 _ 0 = []
chi25 l n = chi2 (rotarFloat 1 l) esp : chi25 l (n-1)

minDL (b:[]) = 1
minDL (b:c:ds) | c >= b = minDL (b:ds)
               | otherwise = 1 + minDL (c:ds)

descifrar :: String -> String
descifrar l = decodificar (-(minDL (chi25 (frec l) 26))) l
