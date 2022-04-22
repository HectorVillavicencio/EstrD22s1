data Pizza = Prepizza
			| Capa Ingrediente Pizza deriving Show
data Ingrediente = Salsa
				| Queso
				| Jamon
				| Aceitunas Int deriving Show

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa ing pizza) = 1 + cantidadDeCapas pizza

armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (x:xs) = Capa x (armarPizza xs)

