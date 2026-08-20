module Laboratorio01 where

distanciaOrigen :: Double -> Double -> Double
distanciaOrigen x y = sqrt(x^2 + y^2)

sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares l = sumaLista (paresLista l)
  where 
    paresLista :: [Int] -> [Int]
    paresLista l = [x | x <- l, even x]

    sumaLista :: [Int] -> Int
    sumaLista [] = 0
    sumaLista (x:xs) = x^2 + sumaLista xs

aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces f x = f (f (f x))

varianza2 :: Double -> Double -> Double
varianza2 x y = sigma/2
  where 
    mu = (x + y)/2
    sigma = (x - mu)^2 + (y - mu)^2

clasificaTemperatura :: Int -> String
clasificaTemperatura n 
                  | n < 1 = "frio extremo"
                  | n  < 16 = "frio"
                  | n  < 26 = "templado"
                  | n  < 36 = "calido"
                  | otherwise = "calor extremo"

intercala :: a -> [a] -> [a]
intercala a [] = []
intercala a [x] = [x]
intercala a (x:xs) = x : a : intercala a xs

data Expr
  = Lit Int
  | Suma Expr Expr
  | Producto Expr Expr
  deriving (Eq, Show)

evalua :: Expr -> Int
evalua (Lit x) = x 
evalua (Suma a b) = evalua a + evalua b
evalua (Producto a b) = evalua a * evalua b
