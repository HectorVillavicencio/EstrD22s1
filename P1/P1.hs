--PARTE 3
--3.1

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


--3.2

data TipoDePokemon = Agua | Fuego | Planta 

data Pokemon = Poke TipoDePokemon Int

data Entrenador = Ent String Pokemon Pokemon

superaA :: Pokemon -> Pokemon -> Bool
superaA  pokemon1 pokemon2 = tipoLeGanaA (tipoDePokemon pokemon1) (tipoDePokemon pokemon2)


-- devuelve el tipo de pokemon del pokemon
tipoDePokemon :: Pokemon -> TipoDePokemon
tipoDePokemon (Poke tipo energia) = tipo


-- devuelve si el primer tipo le gana 
tipoLeGanaA :: TipoDePokemon -> TipoDePokemon -> Bool
tipoLeGanaA Fuego Planta = True
tipoLeGanaA Planta Agua = True
tipoLeGanaA Agua Fuego = True
tipoLeGanaA _ _ =False


cantidadDePokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonesDe tipo (Ent nombre pokemon1 pokemon2) = sumaSiEsMismoTipo tipo (tipoDePokemon pokemon1) + 
															sumaSiEsMismoTipo tipo (tipoDePokemon pokemon2)

sumaSiEsMismoTipo :: TipoDePokemon -> TipoDePokemon -> Int
sumaSiEsMismoTipo Fuego Fuego = 1
sumaSiEsMismoTipo Agua Agua = 1
sumaSiEsMismoTipo Planta Planta = 1
sumaSiEsMismoTipo _ _ = 0

-- parte 4

loMismo :: a -> a
loMismo x = x

siempreSiete :: a -> Int
siempreSiete x = 7

swap :: (a,b) -> (b, a)
swap (x,y) = (y,x)


--PARTE 5

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia (x:xs) = False


--asumiendo que al menos dentro de lalista tiene que haber un 
--elemento, sino tira error
elPrimero :: [a] -> a
elPrimero [] = error "No tiene elementos esta lista"
elPrimero (x:xs) = x

sinElPrimero :: [a] -> [a]
sinElPrimero [] = []
sinElPrimero (x:xs) = xs


splitHead :: [a] -> (a, [a])
splitHead [] = error "No tiene elementos esta lista"
splitHead (x:xs) = (x, xs)

