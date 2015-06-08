import Data.Char 
import Data.String

------------------------------------------ITEM 1----------------------------------------------

-- Lista de minúsculas de la "a" a la "z" para facilitar otras tareas
mIn = ['a'..'z']

-- String para probar funciones
tdA = "taller de algebra"



-- "a" pertenece a una lista [EDIT by santi, quisiera probar esta función, creo que tiene algo mal] <-- ya lo probaste? No le veo lo malo. Leu
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
natALet  n | n <= 25 && n >= 0 = chr $ fromInteger n+97

-- opcional para natALet (mod 26)
--natALet  n = chr $ fromInteger (mod n 26)+97

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
frecLEnL (b:bs) l = (100*(fromInteger $(contar b l))/(fromInteger $(cantMinusc l))) : frecLEnL bs l

frec :: String -> [Float]
frec l = frecLEnL mIn l

------------------------------------------ITEM 5----------------------------------------------

rotar :: Integer -> [a] -> [a]
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


chi25 _ _ 0 = []
chi25 l b n = (chi2 (rotar (b) l) esp) : chi25 l (b+1) (n-1)

minDL (b:[]) = 1
minDL (b:c:ds) | c >= b = minDL (b:ds)
               | otherwise = 1 + minDL (c:ds)



-- Veamos que sale y que opinan de esto.
minimo :: [Float] -> Float
minimo (a:[]) = a
minimo (a:at) 
	| a < minimo at = a
	| otherwise = minimo at

posicionDe :: Float -> [Float] -> Integer
posicionDe x [] = 0   -- En realidad nunca va a llegar a este caso...
posicionDe x (b:bs)
	| x == b = 0
	| otherwise = 1+posicionDe x bs



descifrar :: String -> String
descifrar l = decodificar (posicionDe (minimo listaChi2) listaChi2) l
	where listaChi2 = chi25 (frec l) 0 26
