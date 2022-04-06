

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
agregarAlFinal (x:xs) elem = xs ++ [elem]


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


--3.2 Pokemon

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

cantPokemones :: Entrenador -> Int
cantPokemones (ConsEntrenador nomb pks) = longitud pks

cantPokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonesDe tipo (ConsEntrenador nomb pks) = cantDePokemonesDeTipo tipo pks

cantDePokemonesDeTipo:: TipoDePokemon -> [Pokemon] -> Int
cantDePokemonesDeTipo tipo [] = 0
cantDePokemonesDeTipo tipo (x:xs) = if esMismoTipo tipo (esTipo x)
									then 1 + cantDePokemonesDeTipo tipo xs
									else cantDePokemonesDeTipo tipo xs

esMismoTipo:: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Fuego Fuego = True
esMismoTipo Agua Agua = True
esMismoTipo Planta Planta = True
esMismoTipo _ _ = False


--devuelve el tipo de Pokemon
esTipo:: Pokemon -> TipoDePokemon
esTipo (ConsPokemon tipo vida) = tipo


esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador nomb []) = True
esMaestroPokemon (ConsEntrenador nomb pks) = tieneTipoFuego pks && tieneTipoAgua pks && tieneTipoPlanta pks



--Devuelve True si en la lista hay un pokemon tipo fuego
tieneTipoFuego :: [Pokemon] -> Bool
tieneTipoFuego [] = False
tieneTipoFuego (x:xs) = esMismoTipo Fuego (esTipo x) || tieneTipoFuego xs

--Devuelve True si en la lista hay un Pokemon tipo agua
tieneTipoAgua :: [Pokemon] -> Bool
tieneTipoAgua [] = False
tieneTipoAgua (x:xs) = esMismoTipo Agua (esTipo x) || tieneTipoAgua xs

--Devuelve True si hay al menos un pokemon tipo plnata
tieneTipoPlanta :: [Pokemon] -> Bool
tieneTipoPlanta [] = False
tieneTipoPlanta (x:xs) = esMismoTipo Planta (esTipo x) || tieneTipoPlanta xs


-- 3.3

data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]


proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa []) = []
proyectos (ConsEmpresa (x:xs)) = proyectosDeLosRoles x : proyectos (ConsEmpresa xs)

proyectosDeLosRoles :: Rol -> Proyecto
proyectosDeLosRoles (Developer sen proy) = proy
proyectosDeLosRoles (Management sen proy)= proy



losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa []) ys = 0
losDevSenior (ConsEmpresa (x:xs)) ys = sumarSiEsSenior x + losDevSenior (ConsEmpresa xs) ys

-- devuelve 1 si es 1
sumarSiEsSenior:: Rol -> Int
sumarSiEsSenior (Developer sen proy) = esSenior sen
sumarSiEsSenior (Management sen proy) = esSenior sen


-- retorna 1 si es Senior, si no es Senior devuelve 0
esSenior :: Seniority -> Int
esSenior Senior = 1
esSenior _ = 0


cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn [] (ConsEmpresa ys) = 0
cantQueTrabajanEn (x:xs) (ConsEmpresa ys) = sumarSiEstanEnElProyecto x ys + 
											  cantQueTrabajanEn xs (ConsEmpresa ys)


sumarSiEstanEnElProyecto :: Proyecto -> [Rol] -> Int
sumarSiEstanEnElProyecto proyecto [] = 0
sumarSiEstanEnElProyecto proyecto (x:xs) = sumarSiEstaEnElProyecto proyecto x + sumarSiEstanEnElProyecto proyecto xs


sumarSiEstaEnElProyecto:: Proyecto -> Rol -> Int
sumarSiEstaEnElProyecto (ConsProyecto nomb) (Developer sen proy) =
sumarSiEstaEnElProyecto (ConsProyecto nomb) (Management sen proy) =	