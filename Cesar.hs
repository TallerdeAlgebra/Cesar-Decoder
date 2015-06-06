-- lista de minusculas de la "a" a la "z" para fasilitar otras tareas
min = "abcdefghijklmnopqrstuvwxyz"

-- "a" pertenece a una lista
pertenece a [] = False
pertenece a (b,bs) | a == b || pertenece a bs

esMin a = pertenece a min


-- compara un elemento con una lista y le da un valor segun el primer momento en que aparece
valor a [] = 99999
-- el caso vasio no va a ser utilizado mas que para poder dar un caso bace a la recurrencia
valor a (b,bs) | a == b = 0
			   | otherwise = 1+(valor a bs)

letANat a | esMin a = valor a min
		  | otherwise = a

-- generalizacion de natALet para cualquier lista
toMin 0 (b,bs) = b
toMin n (b,bs) = toMin (n-1) bs
 
natALet n | 25 >= n = toMin n min
		  | otherwise = natALet (n-26) 

desplazar 0 a = a
desplazar n a | esMin a = natALet ((letANat a)+n)
			  | otherwise = a
--
