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
  | If ASA ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  | ExprV ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 3: desazucarado ----------------------------------------------------

-- Recupera estas funciones del laboratorio 4. Las funciones y aplicaciones
-- del nucleo siguen siendo unarias, y las operaciones siguen siendo binarias.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun parametros cuerpo 
  | tieneDuplicados parametros = Nothing
  | otherwise                  = Just (foldr Fun cuerpo parametros)

-- Funcion aux para saber si hay parametros duuplicados
tieneDuplicados :: Eq a => [a] -> Bool
tieneDuplicados [] = False
tieneDuplicados (x:xs) = elem x xs || tieneDuplicados xs

curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp funcion argumentos = Just (foldl App funcion argumentos)

binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ [] = Nothing
binaryOp _ [_] = Nothing
binaryOp operacion (x:xs) = Just (foldl operacion x xs)

-- Desazucara las clausulas ordinarias de cond en If anidados. La alternativa
-- else es el ultimo argumento y se conserva como la rama final.
desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
desugarCond [] ramaElse = desugar ramaElse
desugarCond ((condicion, rama) : resto) ramaElse = construirIF (desugar condicion) (desugar rama) (desugarCond resto ramaElse)

-- Funcion aux para obtener los Just
construirIF :: Maybe ASA -> Maybe ASA -> Maybe ASA -> Maybe ASA
construirIF (Just c) (Just t) (Just e) = Just (If c t e)
construirIF _ _ _                      = Nothing

-- Elimina toda la sintaxis superficial. CondS se traduce a If anidados.
-- LetRecS f definicion cuerpo se traduce usando el identificador Y:
--
--   LetS f (AppS (IdS "Y") (FunS [f] definicion)) cuerpo
--
-- y despues se elimina tambien ese LetS. LetRecS no pertenece al nucleo.
desugar :: SASA -> Maybe ASA
desugar (IdS x) = Just (Id x)
desugar (NumS n) = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)
desugar (AddS operandos) = construirBinaria Add (desugarLista operandos)
desugar (SubS operandos) = construirBinaria Sub (desugarLista operandos)
desugar (NotS x) = construirNot (desugar x)
desugar (LetS x e1 e2) = construirLet x (desugar e1) (desugar e2)
desugar (LetStarS [] cuerpo) = desugar cuerpo
desugar (LetStarS ((x, e1) : resto) cuerpo) = desugar (LetS x e1 (LetStarS resto cuerpo))
desugar (FunS parametros cuerpo) = construirFun parametros (desugar cuerpo)
desugar (AppS funcion parametros) = construirApp (desugar funcion) (desugarLista parametros)
desugar (IfS c t e) = construirIF (desugar c) (desugar t) (desugar e)
desugar (CondS clausulas ramaElse) = desugarCond clausulas ramaElse
desugar (LetRecS f definicion cuerpo) =  desugar (LetS f (AppS (IdS "Y") [(FunS [f] definicion)]) cuerpo)


-- Funciones aux para obtener los Just
construirBinaria :: (ASA -> ASA -> ASA) -> Maybe [ASA] -> Maybe ASA
construirBinaria op (Just args) = binaryOp op args
construirBinaria _ _            = Nothing

desugarLista :: [SASA] -> Maybe [ASA]
desugarLista [] = Just []
desugarLista (x:xs) = construirCons (desugar x) (desugarLista xs)

construirNot :: Maybe ASA -> Maybe ASA
construirNot (Just x) = Just (Not x)
construirNot _        = Nothing

construirLet :: Nombre -> Maybe ASA -> Maybe ASA -> Maybe ASA
construirLet x (Just e1) (Just e2) = Just (App (Fun x e2) e1)
construirLet _ _ _                 = Nothing

construirFun :: [Nombre] -> Maybe ASA -> Maybe ASA
construirFun params (Just cuerpo) = curryFun params cuerpo
construirFun _ _                  = Nothing

construirApp :: Maybe ASA -> Maybe [ASA] -> Maybe ASA
construirApp (Just f) (Just args) = curryApp f args
construirApp _ _                  = Nothing

construirCons :: Maybe ASA -> Maybe [ASA] -> Maybe [ASA]
construirCons (Just x) (Just xs) = Just (x:xs)
construirCons _ _                = Nothing

-- RETO 4: evaluacion perezosa con alcance estatico ------------------------

-- Busca la asociacion mas reciente sin exigir su contenido.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing
lookupEnv x ((n1, v1) : resto) 
  | x == n1 = Just v1
  | otherwise = lookupEnv x resto

-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor.
strict :: Value -> Maybe Value
strict (NumV n) = Just (NumV n)
strict (BooleanV b) = Just (BooleanV b)
strict (ClosureV x e env) = Just (ClosureV x e env)
strict (ExprV e env) = obtenerValor (bigStep env e)

--Funcion aux para obtner un valor de un ExprV
obtenerValor :: Maybe Value -> Maybe Value
obtenerValor (Just v) = strict v
obtenerValor _        = Nothing

-- Semantica de paso grande con alcance estatico y evaluacion perezosa.
--
-- * Id devuelve directamente la asociacion encontrada.
-- * Fun produce ClosureV con el ambiente de definicion.
-- * App exige la posicion de funcion, pero liga el argumento como
--   ExprV argumento ambienteDeLaLlamada.
-- * Add, Sub y Not exigen sus operandos.
-- * If exige solamente la condicion y evalua una sola rama.
--
-- La resta sobre naturales permanece truncada en cero.
bigStep :: Env -> ASA -> Maybe Value
bigStep env (Id x) = lookupEnv x env 
bigStep _ (Num n) = Just (NumV n)
bigStep _ (Boolean b) = Just (BooleanV b)
bigStep env (Add e1 e2) = evaluarAdd (obtenerValor (bigStep env e1)) (obtenerValor (bigStep env e2))
bigStep env (Sub e1 e2) = evaluarSub (obtenerValor (bigStep env e1)) (obtenerValor (bigStep env e2))
bigStep env (Not x) = evaluarNot (obtenerValor (bigStep env x))
bigStep env (Fun parametro cuerpo) = Just (ClosureV parametro cuerpo env)
bigStep env (App funcion parametro) = aplicarApp (ExprV parametro env) (obtenerValor (bigStep env funcion))
bigStep env (If c t e) = evaluarIf env t e (obtenerValor (bigStep env c))

-- Funciones aux para obtener Just
evaluarAdd :: Maybe Value -> Maybe Value -> Maybe Value
evaluarAdd (Just (NumV n1)) (Just (NumV n2)) = Just (NumV (n1 + n2))
evaluarAdd _ _                               = Nothing

evaluarSub :: Maybe Value -> Maybe Value -> Maybe Value
evaluarSub (Just (NumV n1)) (Just (NumV n2)) = Just (NumV (max 0 (n1 - n2)))
evaluarSub _ _                               = Nothing

evaluarNot :: Maybe Value -> Maybe Value
evaluarNot (Just (BooleanV b)) = Just (BooleanV (not b))
evaluarNot (Just (NumV _))     = Just (BooleanV False) 
evaluarNot _                   = Nothing

evaluarIf :: Env -> ASA -> ASA -> Maybe Value -> Maybe Value
evaluarIf env t e (Just (BooleanV True))  = bigStep env t
evaluarIf env t e (Just (BooleanV False)) = bigStep env e
evaluarIf _ _ _ _                         = Nothing

aplicarApp :: Value -> Maybe Value -> Maybe Value
aplicarApp argExpr (Just (ClosureV param cuerpo envCerradura)) =  bigStep ((param, argExpr) : envCerradura) cuerpo
aplicarApp _ _ = Nothing