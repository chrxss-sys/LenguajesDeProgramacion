module Interp where

import Grammars
import Data.Maybe (Maybe(Nothing))

data ASA
  = Id Nombre
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun Nombre ASA
  | App ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 1: desazucarado ----------------------------------------------------

-- Convierte una lista no vacia de parametros distintos en funciones
-- unarias anidadas. El primer parametro queda en la funcion exterior.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun parametros cuerpo 
  | tieneDuplicados parametros = Nothing
  | otherwise                  = Just (foldr Fun cuerpo parametros)

-- Funcion aux para saber si hay parametros duuplicados
tieneDuplicados :: Eq a => [a] -> Bool
tieneDuplicados [] = False
tieneDuplicados (x:xs) = elem x xs || tieneDuplicados xs

-- Convierte una aplicacion con uno o mas argumentos en aplicaciones unarias
-- asociadas por la izquierda.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp funcion argumentos = Just (foldl App funcion argumentos)

-- Convierte dos o mas operandos en operaciones binarias asociadas por la
-- izquierda. El constructor recibido sera Add o Sub.
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ [] = Nothing
binaryOp _ [_] = Nothing
binaryOp operacion (x:xs) = Just (foldl operacion x xs)

-- Convierte las ligaduras de let* en let anidados y despues elimina cada let
-- mediante LetS x e1 e2 ==> App (Fun x e2') e1'. La primera ligadura debe
-- quedar en el let exterior para que las siguientes puedan usarla.
desugar :: SASA -> Maybe ASA
desugar (IdS x) = Just (Id x)
desugar (NumS n) = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)
desugar (AddS operandos) = case desugarLista operandos of
  Just operandosPuros -> binaryOp Add operandosPuros
  _                   -> Nothing

desugar (SubS operandos) = case desugarLista operandos of
  Just operandosPuros -> binaryOp Sub operandosPuros
  _                   -> Nothing

desugar (NotS x) = case desugar x of
  Just x' -> Just (Not x')
  _       -> Nothing

desugar (LetS x e1 e2) = case (desugar e1, desugar e2) of
  (Just e1Puro, Just e2Puro) -> Just (App (Fun x e2Puro) e1Puro)
  _                          -> Nothing

desugar (LetStarS [] cuerpo) = desugar cuerpo
desugar (LetStarS ((x, e1) : resto) cuerpo) = desugar (LetS x e1 (LetStarS resto cuerpo))

desugar (FunS parametros cuerpo) = case desugar cuerpo of
  Just cuerpoPuro -> curryFun parametros cuerpoPuro
  _               -> Nothing

desugar (AppS funcion parametros) = case (desugar funcion, desugarLista parametros) of
  (Just funcionPura, Just parametrosPuros) -> curryApp funcionPura parametrosPuros
  _                                        -> Nothing

-- Funcion aux para desugar listas 
desugarLista :: [SASA] -> Maybe [ASA]
desugarLista [] = Just []
desugarLista (x:xs) = case (desugar x, desugarLista xs) of
  (Just x', Just xs') -> Just (x' : xs')
  _                   -> Nothing


-- RETO 2: evaluacion con cerraduras ---------------------------------------

-- Busca la asociacion mas reciente de un identificador.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing
lookupEnv x ((n1, v1) : resto) 
  | x == n1 = Just v1
  | otherwise = lookupEnv x resto

-- Evalua con alcance estatico. Fun produce una cerradura con el ambiente
-- actual. App evalua primero la posicion de funcion, despues el argumento y
-- por ultimo el cuerpo en el ambiente guardado por la cerradura.
-- La aplicacion es ansiosa: el argumento se exige aunque el cuerpo no lo use.
-- Conserva la resta truncada y la convencion de que todo numero cuenta como
-- verdadero cuando aparece como operando de Not.
bigStep :: Env -> ASA -> Maybe Value
bigStep env (Id x) = lookupEnv x env
bigStep _ (Num n) = Just (NumV n)
bigStep _ (Boolean b) = Just (BooleanV b)
bigStep env (Add e1 e2) = case (bigStep env e1, bigStep env e2) of
  (Just (NumV n1), Just (NumV n2)) -> Just (NumV (n1 + n2))
  _                                -> Nothing

bigStep env (Sub e1 e2) = case (bigStep env e1, bigStep env e2) of
  (Just (NumV n1), Just (NumV n2)) -> Just (NumV (max 0 (n1 - n2)))
  _                                -> Nothing

bigStep env (Not x) = case bigStep env x of
  Just (BooleanV b) -> Just (BooleanV (not b))
  Just (NumV _)     -> Just (BooleanV False)
  _                 -> Nothing

bigStep env (Fun parametro cuerpo) = Just (ClosureV parametro cuerpo env)
bigStep env (App funcion argumento) = case (bigStep env funcion, bigStep env argumento) of
  (Just (ClosureV param cuerpo envCongelado), Just valorArg) -> bigStep ((param, valorArg) : envCongelado) cuerpo
  _                                                          -> Nothing

