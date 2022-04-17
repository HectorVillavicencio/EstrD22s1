-- 1 Recursión sobre listas

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [a] -> Int
longitud [] = 0 
longitud (x:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = x + 1 : sucesores xs

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


agregar :: [a] -> [a] -> [a]
agregar xs [] = xs
agregar [] ys = ys
agregar xs ys = xs ++ ys 

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]


zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] [] = []
zipMaximos (x:xs) (y:ys) = if x<y 
							then y : zipMaximos xs ys
							else x : zipMaximos xs ys

elMinimo :: Ord a => [a] -> a
elMinimo [] = error "tiene que haber al menos un elemento"
elMinimo [x] = x
elMinimo (x:xs) = if x > elMinimo xs
							then x
							else elMinimo xs


--2  Recursión sobre números

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


--3 Registros

data Persona = Pers String Int 

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA num [] = []
mayoresA num (x:xs) = if (edad x) > num
						then x : mayoresA num xs
						else mayoresA num xs

-- esta en la anterior practica
edad :: Persona -> Int
edad (Pers nomb edad) = edad 


promedioEdad :: [Persona] -> Int
promedioEdad xs = div (sumarTodasLasEdades xs) (longitud xs)

--suma todas las edades de las personas de la lista
sumarTodasLasEdades :: [Persona] -> Int
sumarTodasLasEdades [] = 0
sumarTodasLasEdades (x:xs) = edad x + sumarTodasLasEdades xs

--debe haber al menos una persona en la lista
elMasViejo :: [Persona] -> Persona
elMasViejo [x] = x
elMasViejo (x:xs) = elMasViejoDeLosDos x (elMasViejo xs)


--Devuelev cual de las 2 personas es mas grande
elMasViejoDeLosDos :: Persona -> Persona -> Persona
elMasViejoDeLosDos (Pers nomb1 edad1) (Pers nomb2 edad2) = if edad1 > edad2
															then Pers nomb1 edad1
															else Pers nomb2 edad2

-- 2 pokemones
data TipoDePokemon = Agua | Fuego | Planta 
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador nomb poks) = longitud poks

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tipo (ConsEntrenador nomb poks) = sumarLosPokeDeT tipo poks

--suma los pokemones de un tipo predeterminado
sumarLosPokeDeT :: TipoDePokemon -> [Pokemon] -> Int
sumarLosPokeDeT tipo [] = 0
sumarLosPokeDeT tipo (x:xs) = if esMismoTipo tipo (esTipo x)
								then 1 + sumarLosPokeDeT tipo xs
								else sumarLosPokeDeT tipo xs

--me retorna true si los tipos de pokemon son los mismos
esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Agua Agua = True
esMismoTipo Fuego Fuego = True
esMismoTipo Planta Planta = True
esMismoTipo _ _ = False

--devueleve el tipo del pokemon
esTipo :: Pokemon -> TipoDePokemon
esTipo (ConsPokemon tipo ener) = tipo


losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan tipo (ConsEntrenador nomb1 poks1) (ConsEntrenador nomb2 poks2) = if sonDebilesAlTipo tipo poks2
																				then sumarLosPokeDeT tipo poks1
																				else 0
-- retorna true si todos los pokemon son debiles a un tipo
sonDebilesAlTipo :: TipoDePokemon -> [Pokemon] -> Bool
sonDebilesAlTipo tipo [] = True
sonDebilesAlTipo tipo (x:xs) = esDebilAlT tipo (esTipo x)

--devuelve true si el segundo tipo es debila l primero
esDebilAlT :: TipoDePokemon -> TipoDePokemon -> Bool
esDebilAlT Planta Agua = True
esDebilAlT Agua Fuego = True
esDebilAlT Fuego Planta = True
esDebilAlT _ _ = False

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador nomb poks) = tieneTipoAgua poks && tieneTipoFuego poks && tieneTipoPlanta poks

--retorna true si tien al menos un tipo agua en la lista de pokemon
tieneTipoAgua :: [Pokemon] -> Bool
tieneTipoAgua [] = False
tieneTipoAgua (x:xs) = esMismoTipo Agua (esTipo x) || tieneTipoAgua xs

--retorna true si tien al menos un tipo planta en la lista de pokemon
tieneTipoPlanta :: [Pokemon] -> Bool
tieneTipoPlanta [] = False
tieneTipoPlanta (x:xs) = esMismoTipo Planta (esTipo x) || tieneTipoPlanta xs

--retorna true si tien al menos un tipo fuego en la lista de pokemon
tieneTipoFuego :: [Pokemon] -> Bool
tieneTipoFuego [] = False
tieneTipoFuego (x:xs) = esMismoTipo Fuego (esTipo x) || tieneTipoFuego xs

a = ConsPokemon Agua 23
b = ConsPokemon Fuego 23
c = ConsPokemon Planta 23

per = ConsEntrenador "villa" [a,b,c]



data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa roles) = sinRepetidos(sacarProyectosDe roles)

--saca todos los proyectos repetidos
sinRepetidos :: [Proyecto] -> [Proyecto]
sinRepetidos [] = []
sinRepetidos (x:xs) = if perteneceT x xs
						then sinRepetidos xs
						else x : sinRepetidos xs

--devuelve todos los proyectos de los roles
sacarProyectosDe :: [Rol] -> [Proyecto]
sacarProyectosDe [] = []
sacarProyectosDe (x:xs) = sacarProyecto x : sacarProyectosDe xs 


--devueleve el proyecto del rol
sacarProyecto :: Rol -> Proyecto
sacarProyecto (Developer seni proy) = proy
sacarProyecto (Management seni proy) = proy

-------------------------------------------------------------------------------------------------------------------
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa roles) proys = sonDevSeniorYTieneProyecto roles proys

--retornalaCantidad de delevoper senior que tengan algun rolde de la lista										
sonDevSeniorYTieneProyecto :: [Rol] -> [Proyecto] -> Int
sonDevSeniorYTieneProyecto [] ys = 0
sonDevSeniorYTieneProyecto (x:xs) ys = if esDevSeniorYTieneProyecto x ys 
										then 1 + sonDevSeniorYTieneProyecto xs ys
										else sonDevSeniorYTieneProyecto xs ys

										
--retorna true si es un delevoper senior y tiene algun proyecto de la Lista
esDevSeniorYTieneProyecto :: Rol -> [Proyecto] -> Bool
esDevSeniorYTieneProyecto (Developer seni proy) xs = esSenior seni && perteneceT proy xs

-- devuelve true si el rol pertenece a la lista
perteneceT :: Proyecto -> [Proyecto] -> Bool
perteneceT str [] = False
perteneceT str (x:xs) = if sonElMismo (nombreDeProyecto x) (nombreDeProyecto str) 
						then True
						else perteneceT str xs

-- Retorna True si es el mismo elemento
sonElMismo :: String -> String -> Bool
sonElMismo p1 p2 = p1 == p2

--retorna el nombre del proyecto
nombreDeProyecto :: Proyecto -> String 
nombreDeProyecto (ConsProyecto nomb) = nomb


--devuelve true si si el senior
esSenior :: Seniority -> Bool
esSenior Senior = True
esSenior _ = False
------------------------------------------------------------------------------------------------------------------------------------
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn xs (ConsEmpresa roles) = sumarTodosLosProyectos xs (sacarProyectosDe roles)

----retorna la cantidad de veces que estan los proyectos en la lista
sumarTodosLosProyectos :: [Proyecto] -> [Proyecto] -> Int
sumarTodosLosProyectos [] ys = 0
sumarTodosLosProyectos (x:xs) ys = sumarSiTieneproyecto x ys + sumarTodosLosProyectos xs ys

-- retorna la cantidad de veces que se repite el proyecto en la lista
sumarSiTieneproyecto :: Proyecto -> [Proyecto] -> Int
sumarSiTieneproyecto x (y:ys) = if sonElMismo (nombreDeProyecto x) (nombreDeProyecto y)
									then 1 + sumarSiTieneproyecto x ys
									else sumarSiTieneproyecto x ys
------------------------------------------------------------------------------------------------------------------------------------
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa roles) = asignadosPorProyectoDe (proyectos (ConsEmpresa roles)) (sacarProyectosDe roles)

asignadosPorProyectoDe :: [Proyecto] -> [Proyecto] -> [(Proyecto, Int)]
asignadosPorProyectoDe [] ys = []
asignadosPorProyectoDe (x:xs) ys = (x, sumarSiTieneproyecto x ys) : asignadosPorProyectoDe xs ys





