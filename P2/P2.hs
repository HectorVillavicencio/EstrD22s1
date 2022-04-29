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
pertenece elem (x:xs) = elem == x || pertenece elem xs


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
agregar [] ys = ys
agregar (x:xs) ys = x : agregar xs ys

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]




zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] ys = ys
zipMaximos xs [] = xs
zipMaximos (x:xs) (y:ys) = max x y : zipMaximos xs ys  


elMinimo :: Ord a => [a] -> a
elMinimo [] = error "tiene que haber al menos un elemento"
elMinimo [x] = x
elMinimo (x:xs) = min x (elMinimo xs)


--2  Recursión sobre números

factorial :: Int -> Int
factorial 0 = 1
factorial num = num * factorial (num - 1)


cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n = if n <= 0
					then []
					else cuentaRegresiva' n

cuentaRegresiva' :: Int -> [Int]
cuentaRegresiva' 0 = []
cuentaRegresiva' n = n: cuentaRegresiva (n-1)
  

repetir :: Int -> a -> [a]
repetir 0 elem = []
repetir num elem = elem : repetir (num - 1) elem

losPrimeros :: Int -> [a] -> [a]
losPrimeros num [] = []
losPrimeros 0 xs = []
losPrimeros num (x:xs) = x : losPrimeros (num - 1) xs


sinLosPrimeros ::  Int -> [a] -> [a]
sinLosPrimeros n xs = if n < 0
						then xs
						else sinLosPrimeros' n xs

sinLosPrimeros' :: Int -> [a] -> [a]
sinLosPrimeros' n [] = []
sinLosPrimeros' 0 xs = xs 
sinLosPrimeros' n (x:xs) = sinLosPrimeros (n-1) xs 


--mayoresA num [] = []
--mayoresA num (x:xs) = if (edad x) > num
--						then x : mayoresA num xs
--						else mayoresA num xs
data Persona = Pers String Int deriving Show

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
elMasViejoDeLosDos p1 p2 = if edad p1 > edad p2
	                        then p1
	                        else p2

pepito = Pers "pepe" 32
juan = Pers "juan" 15


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
losQueLeGanan tipo (ConsEntrenador nomb1 poks1) (ConsEntrenador nomb2 poks2) = sumarSiLesGananATodos(soloLosDeUnTipo tipo poks1) poks2

sumarSiLesGananATodos :: [Pokemon] -> [Pokemon] -> Int
sumarSiLesGananATodos [] ys = 0
sumarSiLesGananATodos (x:xs) ys = unoSi(leGanaATodos x ys) + sumarSiLesGananATodos xs ys

leGanaATodos :: Pokemon -> [Pokemon] -> Bool
leGanaATodos pok [] = True
leGanaATodos pok (x:xs) =  pokemonLeGanaA pok x && leGanaATodos pok xs

soloLosDeUnTipo ::TipoDePokemon -> [Pokemon] -> [Pokemon]
soloLosDeUnTipo tipo [] = []
soloLosDeUnTipo tipo (x:xs) = if esMismoTipo tipo (esTipo x)
								then x : soloLosDeUnTipo tipo xs
								else soloLosDeUnTipo tipo xs 

--Viene de la practica 1
unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

pokemonLeGanaA :: Pokemon -> Pokemon -> Bool
pokemonLeGanaA (ConsPokemon tipo1 e1) (ConsPokemon tipo2 e2) = esDebilAlT tipo1 tipo2

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
esMaestroPokemon (ConsEntrenador nomb poks) = tieneTipo Agua poks && tieneTipo Fuego poks && tieneTipo Planta poks

tieneTipo :: TipoDePokemon -> [Pokemon]-> Bool
tieneTipo t [] = False
tieneTipo t (x:xs) = esMismoTipo t (esTipo x) || tieneTipo t xs


a = ConsPokemon Agua 23
b = ConsPokemon Fuego 23
c = ConsPokemon Planta 23

per = ConsEntrenador "villa" [a,b,c]



data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa roles) = sinRepetidos(sacarProyectosDe roles)

--saca todos los proyectos repetidos
sinRepetidos :: [Proyecto] -> [Proyecto]
sinRepetidos [] = []
sinRepetidos (x:xs) = sacarRepetido x xs ++ sinRepetidos xs


sacarRepetido ::Proyecto -> [Proyecto] -> [Proyecto]
sacarRepetido p xs = if tieneProyecto p xs
						then []
						else [p]

sonElMismoP :: Proyecto -> Proyecto -> Bool
sonElMismoP (ConsProyecto n1) (ConsProyecto n2) = n1 == n2

--devuelve todos los proyectos de los roles
sacarProyectosDe :: [Rol] -> [Proyecto]
sacarProyectosDe [] = []
sacarProyectosDe (x:xs) = sacarProyecto x : sacarProyectosDe xs 

pro1 = ConsProyecto "gatito"
pro2 = ConsProyecto "perrito"
pro3 = ConsProyecto "perico"


--devueleve el proyecto del rol
sacarProyecto :: Rol -> Proyecto
sacarProyecto (Developer seni proy) = proy
sacarProyecto (Management seni proy) = proy

-------------------------------------------------------------------------------------------------------------------
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa roles) proys = cantidadDevSeniorConProyecto roles proys

--retornalaCantidad de delevoper senior que tengan algun rolde de la lista	
--sonDevSeniorYTieneProyecto									
cantidadDevSeniorConProyecto :: [Rol] -> [Proyecto] -> Int
cantidadDevSeniorConProyecto [] ys = 0
cantidadDevSeniorConProyecto (x:xs) ys = if esDevSeniorYTieneProyecto x ys 
										then 1 + cantidadDevSeniorConProyecto xs ys
										else cantidadDevSeniorConProyecto xs ys

										
--retorna true si es un delevoper senior y tiene algun proyecto de la Lista
esDevSeniorYTieneProyecto :: Rol -> [Proyecto] -> Bool
esDevSeniorYTieneProyecto (Developer seni proy) xs = esSenior seni && tieneProyecto proy xs

-- devuelve true si el rol pertenece a la lista
-- reemplazo a perteneceT
tieneProyecto :: Proyecto -> [Proyecto] -> Bool
tieneProyecto proy [] = False
tieneProyecto proy (x:xs) = nombreDeProyecto proy == nombreDeProyecto x || tieneProyecto proy xs


-- Retorna True si es el mismo elemento
sonElMismo :: String -> String -> Bool
sonElMismo p1 p2 = p1 == p2

--retorna el nombre del proyecto
nombreDeProyecto :: Proyecto -> String 
nombreDeProyecto (ConsProyecto nomb) = nomb


--devuelve true si es el senior
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
asignadosPorProyecto (ConsEmpresa roles) = proyectosConSusInvolucrados roles

proyectosConSusInvolucrados :: [Rol] -> [(Proyecto, Int)]
proyectosConSusInvolucrados [] = []
proyectosConSusInvolucrados (x:xs) = agregarProyectoDe x (proyectosConSusInvolucrados xs)

agregarProyectoDe :: Rol -> [(Proyecto, Int)] -> [(Proyecto, Int)]
agregarProyectoDe rol [] = [(sacarProyecto rol, 1)] 
agregarProyectoDe rol ((x,n):xs) = if sonElMismo (sacarNombreDelProyectosDe rol) (nombreDeProyecto x)
										then (x ,n+1) : xs
										else (x,n) : agregarProyectoDe rol xs

sacarNombreDelProyectosDe :: Rol -> String 
sacarNombreDelProyectosDe rol = nombreDeProyecto (sacarProyecto rol)







