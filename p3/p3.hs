-- Ripos recursivos simples
--1.1 Celdas con bolitas
data Color = Azul | Rojo
data Celda = Bolita Color Celda | CeldaVacia 

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
poner color CeldaVacia = Bolita color CeldaVacia
poner color (Bolita colorc celda) = Bolita colorc (poner color celda)


sacar :: Color -> Celda -> Celda
sacar color CeldaVacia = CeldaVacia
sacar color (Bolita colorc celda) = if esMismocolor color colorc
										then sacar color celda
										else Bolita colorc (sacar color celda)


ponerN :: Int -> Color -> Celda -> Celda
ponerN n color CeldaVacia = CeldaVacia
ponerN n color (Bolita colorc celda) = if n > 0
										then Bolita color (ponerN (n-1) color celda)
										else ponerN n color celda

--1.2 Camino hacia el tesoro

data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada camino) = False || hayTesoro camino 
hayTesoro (Cofre xs camino) = tieneTesoro xs || hayTesoro camino

--devueleve true si al menos hay un tesoro en la lista
tieneTesoro :: [Objeto] -> Bool
tieneTesoro [] = False
tieneTesoro (x:xs) = esTesoro x || tieneTesoro xs

--devueleve true si es tesoro
esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False
-- esta mal echo, falta recurcion
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = 0
pasosHastaTesoro (Nada camino) = 1 + pasosHastaTesoro camino
pasosHastaTesoro (Cofre xs camino) = if tieneTesoro xs
									then pasosHastaTesoro Fin
									else 1 + pasosHastaTesoro camino



hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n Fin = False
hayTesoroEn n (Nada camino) = False 
hayTesoroEn n (Cofre xs camino) = if n > 0
									then 2
									else 2
