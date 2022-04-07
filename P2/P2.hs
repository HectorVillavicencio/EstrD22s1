

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [a] -> Int
longitud [] = 0 
longitud (x:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = x+1 : sucesores xs

conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (x:xs) = x && conjuncion xs

disyuncion :: [Bool] -> Bool 
disyuncion [] = False
disyuncion (x:xs) = x || disyuncion xs

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece elem [] = False
pertenece elem (x:xs) = if elem == x 
						then True
						else pertenece elem xs


apariciones :: Eq a => a -> [a] -> Int
apariciones elem [] = 0
apariciones elem (x:xs) = if elem == x
							then 1 + apariciones elem xs
							else apariciones elem xs


losMenoresA :: Int -> [Int] -> [Int]
losMenoresA num [] = []
losMenoresA num (x:xs) = if x < num
							then x : losMenoresA num xs
							else losMenoresA num xs


lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA num [] = []
lasDeLongitudMayorA num (x:xs) = if num < longitud x
									then x : lasDeLongitudMayorA num xs
									else lasDeLongitudMayorA num xs


agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] elem = [elem]
agregarAlFinal (x:xs) elem = x:agregarAlFinal xs elem


concatenar :: [a] -> [a] -> [a]
concatenar xs [] = xs
concatenar [] ys = ys
concatenar xs ys = xs ++ ys 

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]


zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] [] = []
zipMaximos (x:xs) (y:ys) = if x<y 
							then y : zipMaximos xs ys
							else x : zipMaximos xs ys

--elMinimo :: Ord a => [a] -> a
--elMinimo [] = error "tiene que haber al menos un elemento"
--elMinimo (x:xs) = 


--PARTE 2

factorial :: Int -> Int
factorial 0 = 1
factorial num = (num * num) + factorial (num - 1)


cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 1 = [1]
cuentaRegresiva num = num : cuentaRegresiva (num - 1)


repetir :: Int -> a -> [a]
repetir 0 elem = []
repetir num elem = elem : repetir (num - 1) elem

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 (x:xs) = []
losPrimeros num [] = []
losPrimeros num (x:xs) = x : losPrimeros (num - 1) xs


sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros num [] = []
sinLosPrimeros num (x:xs) = if num > 0
							then sinLosPrimeros (num -1) xs
							else x : sinLosPrimeros (num -1) xs


--PARTE 3

data Persona = Pers String Int 

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA num [] = []
mayoresA num (x:xs) = if (edad x) > num
						then x : mayoresA num xs
						else mayoresA num xs

edad :: Persona -> Int
edad (Pers nomb edad) = edad 


promedioEdad :: [Persona] -> Int
promedioEdad xs = div (sumarTodasLasEdades xs) (longitud xs)

sumarTodasLasEdades :: [Persona] -> Int
sumarTodasLasEdades [] = 0
sumarTodasLasEdades (x:xs) = edad x + sumarTodasLasEdades xs

elMasViejo :: [Persona] -> Persona
elMasViejo [x] = x
elMasViejo (x:xs) = elMasViejodeDos x (elMasViejo xs)


elMasViejodeDos :: Persona -> Persona -> Persona
elMasViejodeDos (Pers nomb edad) (Pers nomb2 edad2) = if edad > edad2
														then Pers nomb edad
														else Pers nomb2 edad2


