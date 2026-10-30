module Laboratorio06 where

-- 1. Potencia --------------------------------------------------------------

-- Precondicion: el exponente es mayor o igual que cero.
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base exponente =
  base * potencia base (exponente - 1)

potenciaCola :: Int -> Int -> Int
potenciaCola base exponente = potenciaCola' base exponente 1

potenciaCola' :: Int -> Int -> Int -> Int
potenciaCola' _ 0 acc = acc
potenciaCola' base exponente acc = potenciaCola' base (exponente - 1) (acc * base)

potenciaCPS :: Int -> Int -> (Int -> r) -> r
potenciaCPS _ 0 k = k 1
potenciaCPS base exponente k = potenciaCPS base (exponente - 1) (\v -> k (base * v))

-- 2. Longitud de una lista -------------------------------------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs

longitudCola :: [a] -> Int
longitudCola lista = longitudCola' lista 0

longitudCola' :: [a] -> Int -> Int
longitudCola' [] acc = acc
longitudCola' (x:xs) acc = longitudCola' xs (acc + 1)

longitudCPS :: [a] -> (Int -> r) -> r
longitudCPS [] k = k 0
longitudCPS (x:xs) k = longitudCPS xs (\v -> k (1 + v))

-- 3. Reversa de una lista --------------------------------------------------

reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x]

reversaCola :: [a] -> [a]
reversaCola lista = reversaCola' lista []

reversaCola' :: [a] -> [a] -> [a]
reversaCola' [] acc = acc
reversaCola' (x:xs) acc = reversaCola' xs (x : acc)

reversaCPS :: [a] -> ([a] -> r) -> r
reversaCPS [] k = k []
reversaCPS (x:xs) k = reversaCPS xs (\v -> k (v ++ [x]))

-- 4. Concatenacion de una lista de listas ---------------------------------

concatena :: [[a]] -> [a]
concatena [] = []
concatena (xs : xss) = xs ++ concatena xss

concatenaCola :: [[a]] -> [a]
concatenaCola lista = concatenaCola' lista []

concatenaCola' :: [[a]] -> [a] -> [a]
concatenaCola' [] acc = acc
concatenaCola' (xs : xss) acc = concatenaCola' xss (acc ++ xs)

concatenaCPS :: [[a]] -> ([a] -> r) -> r
concatenaCPS [] k = k []
concatenaCPS (xs : xss) k = concatenaCPS xss (\v -> k (xs ++ v))

-- 5. Intercalado de dos listas --------------------------------------------

intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x : xs) (y : ys) =
  x : y : intercala xs ys

intercalaCola :: [a] -> [a] -> [a]
intercalaCola lista1 lista2 = intercalaCola' lista1  lista2 []

intercalaCola' :: [a] -> [a] -> [a] -> [a]
intercalaCola' [] [] acc = acc
intercalaCola' [] ys acc = acc ++ ys
intercalaCola' xs [] acc = acc ++ xs
intercalaCola' (x:xs) (y:ys) acc = intercalaCola' xs ys (acc ++ [x, y])

intercalaCPS :: [a] -> [a] -> ([a] -> r) -> r
intercalaCPS [] [] k = k []
intercalaCPS [] ys k = k ys
intercalaCPS xs [] k = k xs
intercalaCPS (x:xs) (y:ys) k = intercalaCPS xs ys (\v -> k (x : y : v))