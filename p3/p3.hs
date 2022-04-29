-- Ripos recursivos simples
--1.1 Celdas con bolitas
data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show

nroBolitas :: Color -> Celda -> Int
nroBolitas color CeldaVacia = 0
nroBolitas color (Bolita colorc celda) = if esMismocolor color colorc
											then 1 + nroBolitas color celda
											else nroBolitas color celda

-- devueleve true si las bolitas son del mismo color
esMismocolor :: Color -> Color -> Bool
esMismocolor Rojo Rojo = True
esMismocolor Azul Azul = True
esMismocolor _ _ = False

poner :: Color -> Celda -> Celda
poner color celda = Bolita color celda



sacar :: Color -> Celda -> Celda
sacar color CeldaVacia = CeldaVacia
sacar color (Bolita colorc celda) = if esMismocolor color colorc 
										then celda
										else Bolita colorc (sacar color celda)





ponerN :: Int -> Color -> Celda -> Celda
ponerN n col celda = if n < 0
						then celda
						else ponerSi n col celda

ponerSi :: Int-> Color -> Celda -> Celda
ponerSi 0 col celda = celda
ponerSi n col celda = poner col (ponerSi (n-1) col celda)

		
celdita = Bolita Rojo (Bolita Azul (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia))))

--1.2 Camino hacia el tesoro

data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada camino) = hayTesoro camino 
hayTesoro (Cofre xs camino) = tieneTesoro xs || hayTesoro camino

--devueleve true si al menos hay un tesoro en la lista
tieneTesoro :: [Objeto] -> Bool
tieneTesoro [] = False
tieneTesoro (x:xs) = esTesoro x || tieneTesoro xs

--devueleve true si es tesoro
esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = 0
pasosHastaTesoro (Nada camino) = 1 + pasosHastaTesoro camino
pasosHastaTesoro (Cofre xs camino) = if tieneTesoro xs
									then 0
									else 1 + pasosHastaTesoro camino


hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 camino = mirarSihayTesoroEnLaPrimeraPosicion camino
hayTesoroEn n Fin = False
hayTesoroEn n (Nada camino) = hayTesoroEn (n-1) camino  
hayTesoroEn n (Cofre xs camino) = hayTesoroEn (n-1) camino

mirarSihayTesoroEnLaPrimeraPosicion :: Camino -> Bool
mirarSihayTesoroEnLaPrimeraPosicion Fin = False
mirarSihayTesoroEnLaPrimeraPosicion (Nada camino) = False
mirarSihayTesoroEnLaPrimeraPosicion (Cofre xs camino) = tieneTesoro xs 


alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n Fin = n < 0
alMenosNTesoros n (Nada camino) = alMenosNTesoros n camino 
alMenosNTesoros n (Cofre xs camino) = alMenosNTesoros (n - (sumarLosTesoros xs)) camino


sumarLosTesoros :: [Objeto] -> Int
sumarLosTesoros [] = 0
sumarLosTesoros (x:xs) = unoSi(esTesoro x) + sumarLosTesoros xs

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

--cantTesorosEntre :: Int -> Int -> Camino -> Int
--cantTesorosEntre r1  r2 

--2 Tipos de Arboles
--2.1
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show


sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT a izq der) = a + sumarT izq + sumarT der

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT a izq der) = 1 + sizeT izq + sizeT der

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT a izq der) = NodeT (a*2) (mapDobleT izq) (mapDobleT der)

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT a1 EmptyT = False
perteneceT a1 (NodeT a2 izq der) = (a1 == a2) || perteneceT a1 izq || perteneceT a1 der

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT a1 EmptyT = 0
aparicionesT a1 (NodeT a2 izq der) = unoSi(sonElMismo a1 a2) + aparicionesT a1 izq + aparicionesT a1 der 

sonElMismo :: Eq a => a -> a -> Bool
sonElMismo a1 a2 = a1 == a2
							
							

