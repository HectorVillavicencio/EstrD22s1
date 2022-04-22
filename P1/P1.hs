--Parte 1

--Parte 2
--1
sucesor :: Int -> Int
sucesor n = n+1

sumar:: Int -> Int -> Int
sumar n m = n+m

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n m, mod n m)


maxDelPar :: (Int,Int) -> Int
maxDelPar (n,m) = if n>m
				then n
				else m

--2

-- maxDelPar (divisionYResto (sumar 8 2) (sucesor 49))
-- sucesor (sumar (maxDelPar (divisionYResto 9 1)) 0) 
-- sucesor (maxDelPar (divisionYResto (sumar 8 1) (1)))
-- sumar 1 (maxDelPar(divisionYResto (sumar 8 10) (sucesor 1)))

--3 Tipos enumarativos 
--3.1
data Dir = Norte|Sur|Este|Oeste

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Oeste = Este
opuesto Este = Oeste

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False

-- siguiente es total y no tiene precondiciones ya que cualquier direccion que le des siempre tendra un resultado
siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = Norte

--3.2
data DiaDeSemana = Lunes|Martes|Miercoles|Jueves|Viernes|Sabado|Domingo

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes,Domingo)

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Miercoles = True
empiezaConM Martes = True
empiezaConM _ = False

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues dia1 dia2 = numeroDelDia dia1 > numeroDelDia dia2

--Ordena los dias donde lunes es 1 hasta el domingo hasta 7
numeroDelDia :: DiaDeSemana -> Int
numeroDelDia Lunes = 1
numeroDelDia Martes = 2
numeroDelDia Miercoles = 3
numeroDelDia Jueves = 4
numeroDelDia Viernes = 5
numeroDelDia Sabado = 6
numeroDelDia domingo = 7

--3.3
negar:: Bool -> Bool
negar True = False
negar False = True

implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

yTambien :: Bool -> Bool -> Bool
yTambien True b = b
yTambien False _ = False

oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien False b = b 



--4 Registros
--4.1

data Persona = Consp String Int 

nombre :: Persona -> String
nombre (Consp nombre edad) = nombre


edad :: Persona -> Int
edad (Consp nombre edad) = edad


crecer :: Persona -> Persona
crecer (Consp nombre edad) = Consp nombre (edad + 1)


cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nuevoNombre (Consp nombre edad) = Consp nuevoNombre edad


esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra persona1 persona2 = edad persona1 > edad persona2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor persona1 persona2 = if esMayorQueLaOtra persona1 persona2
									then persona1
									else persona2


--4.2

data TipoDePokemon = Agua | Fuego | Planta 

data Pokemon = Poke TipoDePokemon Int

data Entrenador = Ent String Pokemon Pokemon

superaA :: Pokemon -> Pokemon -> Bool
superaA  pokemon1 pokemon2 = tipoLeGanaA (tipoDePokemon pokemon1) (tipoDePokemon pokemon2)


-- devuelve el tipo de pokemon del pokemon
tipoDePokemon :: Pokemon -> TipoDePokemon
tipoDePokemon (Poke tipo energia) = tipo


-- devuelve True si el primer tipo le gana al otro
tipoLeGanaA :: TipoDePokemon -> TipoDePokemon -> Bool
tipoLeGanaA Fuego Planta = True
tipoLeGanaA Planta Agua = True
tipoLeGanaA Agua Fuego = True
tipoLeGanaA _ _ =False


cantidadDePokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonesDe tipo (Ent nombre pokemon1 pokemon2) = sumaSiEsMismoTipo tipo (tipoDePokemon pokemon1) + 
															sumaSiEsMismoTipo tipo (tipoDePokemon pokemon2)

sumaSiEsMismoTipo :: TipoDePokemon -> TipoDePokemon -> Int
sumaSiEsMismoTipo tipo1 tipo2 = unoSi(esMismoTipo tipo1 tipo2)
								

-- 
unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

--devuelve True si son el mismo tipo
esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Fuego Fuego =  True
esMismoTipo Agua Agua = True
esMismoTipo Planta Planta = True
esMismoTipo _ _=False 


-- 5 Funciones Polimorficas

loMismo :: a -> a
loMismo x = x

siempreSiete :: a -> Int
siempreSiete x = 7

swap :: (a,b) -> (b, a)
swap (x,y) = (y,x)


--PARTE 6

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia (x:xs) = False


--asumiendo que al menos dentro de lalista tiene que haber un 
--elemento, sino tira error
elPrimero :: [a] -> a
elPrimero [] = error "No tiene elementos esta lista"
elPrimero (x:xs) = x

sinElPrimero :: [a] -> [a]
sinElPrimero [] = error "debe haber al menos un elemento en la lista"
sinElPrimero (x:xs) = xs


splitHead :: [a] -> (a, [a])
splitHead [] = error "No tiene elementos esta lista"
splitHead (x:xs) = (x, xs)

