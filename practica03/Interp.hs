module Interp where

import Grammars

-- RETO 3: sustitucion nominal que evita captura
freeVars :: ASA -> [String]
freeVars (Id x) = [x]
freeVars (Num _) = []
freeVars (Boolean _) = []
freeVars (And lista) = concatMap freeVars lista
freeVars (Or lista) = concatMap freeVars lista
freeVars (Add lista) = concatMap freeVars lista
freeVars (Sub lista) = concatMap freeVars lista
freeVars (Mul lista) = concatMap freeVars lista
freeVars (Div lista) = concatMap freeVars lista
freeVars (Lt lista) = concatMap freeVars lista
freeVars (Gt lista) = concatMap freeVars lista
freeVars (Le lista) = concatMap freeVars lista
freeVars (Ge lista) = concatMap freeVars lista
freeVars (Expt e1 e2) = freeVars e1 ++ freeVars e2
freeVars (EqP e1 e2) = freeVars e1 ++ freeVars e2
freeVars (Not e) = freeVars e
freeVars (Add1 e) = freeVars e
freeVars (Sub1 e) = freeVars e
freeVars (ZeroP e) = freeVars e
freeVars (Let bindings cuerpo) = freeVarsExpresiones ++ freeVarsCuerpoFiltrado
    where
        -- -- Sacamos las variables ligadoras del primer elemento de cada tupla (Binding) en bindings
        varsLigadoras = map fst bindings 
        -- Sacamos las variables libres la expresion de cada tupla (binding) en bindings
        freeVarsExpresiones = concatMap (\tupla -> freeVars (snd tupla)) bindings
        freeVarsCuerpo = freeVars cuerpo
        -- Quitamos todas las variables ligadoras del conjunto de variables del cuerpo
        freeVarsCuerpoFiltrado = filter (\var -> notElem var varsLigadoras) freeVarsCuerpo
freeVars (LetStar [] cuerpo) = freeVars cuerpo
freeVars (LetStar ((x, expr1) : bindingsRestantes) cuerpo) = freeVarsExpr1 ++ freeVarsRestoFiltrado
    where
        -- Obtenemos las variables libres de la expresion de la primera tupla
        freeVarsExpr1         = freeVars expr1
        -- Aplicamos recursion a las demas tuplas
        freeVarsResto         = freeVars (LetStar bindingsRestantes cuerpo)
        -- filtramos las variables libres con cada variable ligadora
        freeVarsRestoFiltrado = filter (\v -> v /= x) freeVarsResto



names :: ASA -> [String]
names (Id x) = []
names (Num _) = []
names (Boolean _) = []
names (And lista) = concatMap names lista
names (Or lista) = concatMap names lista
names (Add lista) = concatMap names lista
names (Sub lista) = concatMap names lista
names (Mul lista) = concatMap names lista
names (Div lista) = concatMap names lista
names (Lt lista) = concatMap names lista
names (Gt lista) = concatMap names lista
names (Le lista) = concatMap names lista
names (Ge lista) = concatMap names lista
names (Expt e1 e2) = names e1 ++ names e2
names (EqP e1 e2) = names e1 ++ names e2
names (Not e) = names e
names (Add1 e) = names e
names (Sub1 e) = names e
names (ZeroP e) = names e
names (Let bindings cuerpo) = ligadoresLocales ++ ligadoresEnExp ++ ligadoresCuerpo
    where
        ligadoresLocales = map fst bindings
        ligadoresEnExp = concatMap (\(_, e) -> names e) bindings
        ligadoresCuerpo = names cuerpo
names (LetStar bindings cuerpo) = ligadoresLocales ++ ligadoresEnExp ++ ligadoresCuerpo
    where
        ligadoresLocales = map fst bindings
        ligadoresEnExp = concatMap (\(_, e) -> names e) bindings
        ligadoresCuerpo = names cuerpo

freshName :: [String] -> String
freshName nombresUsados = aux 0
    where
        aux n 
            | notElem sufijo nombresUsados = sufijo
            | otherwise = aux (n + 1)
            where
                sufijo = "v" ++ show n

sust :: ASA -> String -> ASA -> ASA
sust (Id y) x s
    | y == x    = s
    | otherwise = Id y
sust (Num n) x s = Num n
sust (Boolean b) x s = Boolean b
sust (And lista) x s = And (map (\e -> sust e x s) lista)
sust (Or lista) x s = Or (map (\e -> sust e x s) lista)
sust (Add lista) x s = Add (map (\e -> sust e x s) lista)
sust (Sub lista) x s = Sub (map (\e -> sust e x s) lista)
sust (Mul lista) x s = Mul (map (\e -> sust e x s) lista)
sust (Div lista) x s = Div (map (\e -> sust e x s) lista)
sust (Lt lista) x s = Lt (map (\e -> sust e x s) lista)
sust (Gt lista) x s = Gt (map (\e -> sust e x s) lista)
sust (Le lista) x s = Le (map (\e -> sust e x s) lista)
sust (Ge lista) x s = Ge (map (\e -> sust e x s) lista)
sust (Expt e1 e2) x s = Expt (sust e1 x s) (sust e2 x s)
sust (EqP e1 e2) x s = EqP (sust e1 x s) (sust e2 x s)
sust (Not e) x s = Not (sust e x s)
sust (Add1 e) x s = Add1 (sust e x s)
sust (Sub1 e) x s = Sub1 (sust e x s)
sust (ZeroP e) x s = ZeroP (sust e x s)
sust (Let bindings cuerpo) x s
    -- Si y = x
    | elem x (map fst bindings) = 
        Let (map (\(y, ei) -> (y, sust ei x s)) bindings) cuerpo

    -- Si y != x, y pertenece a FV(s)
    | any (\(y, _) -> elem y (freeVars s)) bindings = 
        sust (Let bindingsRenombrados cuerpoRenombrado) x s

    -- Si y != x , y no pertenece a FV(s)
    | otherwise = 
        Let (map (\(y, ei) -> (y, sust ei x s)) bindings) (sust cuerpo x s)

        where
            -- obtenemos la variable conflictiva
            (y, _) = head (filter (\(y', _) -> elem y' (freeVars s)) bindings)
            -- obtenemos una variable fresca a partir de las variables prohibidas
            z = freshName (names (Let bindings cuerpo) ++ freeVars s ++ [x])
            cuerpoRenombrado = sust cuerpo y (Id z)
            bindingsRenombrados = map (\(yi, ei) -> if yi == y then (z, ei) else (yi, ei)) bindings

sust (LetStar [] cuerpo) x s = LetStar [] (sust cuerpo x s)
sust (LetStar ((y, e1) : resto) cuerpo) x s
    -- Si y == x
    | y == x = 
        LetStar ((y, sust e1 x s) : resto) cuerpo -- Solo sustituimos en la primera tupla

    -- Si y != x, y pertenece a Fv(s)
    | elem y (freeVars s) = 
        sust (LetStar ((z, e1) : restoRenombrado) cuerpoRenombrado) x s

    -- Si y != x, y no pertenece a Fv(s)
    | otherwise = 
        LetStar ((y, sust e1 x s) : restoNuevo) cuerpoNuevo        

        where
            -- para: Si y != x, y pertenece a Fv(s)
            z = freshName (names (LetStar resto cuerpo) ++ freeVars s ++ [x])
            cuerpoRenombrado = sust cuerpo y (Id z)
            restoRenombrado = map (\(yi, ei) -> (yi, sust ei y (Id z))) resto
            -- para otherwise
            LetStar restoNuevo cuerpoNuevo = sust (LetStar resto cuerpo) x s    

sustMany :: ASA -> [Binding] -> ASA
sustMany (Id x) sustituciones
    | null coincidencias = Id x
    | otherwise          = snd (head coincidencias)
    where
        coincidencias = filter (\(var, _) -> var == x) sustituciones
sustMany (Num n) sustituciones = Num n
sustMany (Boolean b) sustituciones = Boolean b
sustMany (And lista) sustituciones = And (map (\e -> sustMany e sustituciones) lista)
sustMany (Or lista) sustituciones = Or (map (\e -> sustMany e sustituciones) lista)
sustMany (Add lista) sustituciones = Add (map (\e -> sustMany e sustituciones) lista)
sustMany (Sub lista) sustituciones = Sub (map (\e -> sustMany e sustituciones) lista)
sustMany (Mul lista) sustituciones = Mul (map (\e -> sustMany e sustituciones) lista)
sustMany (Div lista) sustituciones = Div (map (\e -> sustMany e sustituciones) lista)
sustMany (Lt lista) sustituciones = Lt (map (\e -> sustMany e sustituciones) lista)
sustMany (Gt lista) sustituciones = Gt (map (\e -> sustMany e sustituciones) lista)
sustMany (Le lista) sustituciones = Le (map (\e -> sustMany e sustituciones) lista)
sustMany (Ge lista) sustituciones = Ge (map (\e -> sustMany e sustituciones) lista)
sustMany (Expt e1 e2) sustituciones = Expt (sustMany e1 sustituciones) (sustMany e2 sustituciones)
sustMany (EqP e1 e2) sustituciones = EqP (sustMany e1 sustituciones) (sustMany e2 sustituciones)
sustMany (Not e) sustituciones = Not (sustMany e sustituciones)
sustMany (Add1 e) sustituciones = Add1 (sustMany e sustituciones)
sustMany (Sub1 e) sustituciones = Sub1 (sustMany e sustituciones)
sustMany (ZeroP e) sustituciones = ZeroP (sustMany e sustituciones)
sustMany (Let bindings cuerpo) sustituciones =
    Let bindingsSustituidos cuerpoSustituido
    where
        -- a la expresion de cada tupla le hacemos sustMany
        bindingsSustituidos = map (\(y, ei) -> (y, sustMany ei sustituciones)) bindings
        ligadores = map fst bindings
        -- conservamos las sutituciones tales que su variable no este en los ligadores
        sustitucionesFiltradas = filter (\(var, _) -> notElem var ligadores) sustituciones
        cuerpoSustituido = sustMany cuerpo sustitucionesFiltradas
sustMany (LetStar [] cuerpo) sustituciones = LetStar [] (sustMany cuerpo sustituciones)
sustMany (LetStar ((y, e1) : rest) cuerpo) sustituciones =
    LetStar ((y, e1Sustituido) : restoSustituido) cuerpoSustituido
    where
        -- sustituimos la expresion de la primera tupla con las sustituciones completas
        e1Sustituido = sustMany e1 sustituciones
        
        -- filtramos a 'y' para que no afecte a lo que sigue
        sustitucionesFiltradas = filter (\(var, _) -> var /= y) sustituciones
        
        -- el resto y el cuerpo usan la lista filtrada
        LetStar restoSustituido cuerpoSustituido = sustMany (LetStar rest cuerpo) sustitucionesFiltradas

-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.
bigStep :: ASA -> Maybe ASA
bigStep (Id _)= Nothing
bigStep (Num n) = Just (Num n)
bigStep (Boolean b) = Just (Boolean b)

bigStep (And lista) = 
    case evaluarBooleanos lista of
        Just booleanos                      -> Just (Boolean (and booleanos))
        _                                   -> Nothing 

bigStep (Or lista) = 
    case evaluarBooleanos lista of
        Just booleanos                      -> Just (Boolean (or booleanos))
        _                                   -> Nothing                       

bigStep (Add lista) = 
    case evaluarNumeros lista of
        Just numeros                        -> Just (Num (sum numeros))
        _                                   -> Nothing 

bigStep (Sub lista) = 
    case evaluarNumeros lista of
        Just (n:ns) -> Just (Num (foldl (\acc x -> max 0 (acc - x)) n ns))
        _           -> Nothing

bigStep (Mul lista) = 
    case evaluarNumeros lista of
        Just numeros                        -> Just (Num (product numeros))
        _                                   -> Nothing 

bigStep (Div lista) = 
    case evaluarNumeros lista of
        Just (n:ns) -> 
            if elem 0 ns 
            then Nothing 
            else Just (Num (foldl div n ns))
        _ -> Nothing

bigStep (Lt lista) = 
    case evaluarNumeros lista of
        Just numeros -> Just (Boolean (verificarRelacion (<) numeros))
        Nothing      -> Nothing

bigStep (Gt lista) = 
    case evaluarNumeros lista of
        Just numeros -> Just (Boolean (verificarRelacion (>) numeros))
        Nothing      -> Nothing

bigStep (Le lista) = 
    case evaluarNumeros lista of
        Just numeros -> Just (Boolean (verificarRelacion (<=) numeros))
        Nothing      -> Nothing

bigStep (Ge lista) = 
    case evaluarNumeros lista of
        Just numeros -> Just (Boolean (verificarRelacion (>=) numeros))
        Nothing      -> Nothing

bigStep (Expt e1 e2) = 
    case (bigStep e1, bigStep e2) of
        (Just (Num n1), Just (Num n2))      -> Just (Num (n1 ^ n2))
        _                                   -> Nothing 

bigStep (EqP e1 e2) = 
    case (bigStep e1, bigStep e2) of
        (Just (Num n1), Just (Num n2))      -> Just (Boolean (n1 == n2))
        _                                   -> Nothing 

bigStep (Not e) =
    case bigStep e of
        Just (Boolean b)    -> Just (Boolean (not b))
        _                   -> Nothing

bigStep (Add1 e) = 
    case bigStep e of
        Just (Num n)        -> Just ( Num (n+1))
        _                   -> Nothing

bigStep (Sub1 e) = 
    case bigStep e of
        Just (Num n)        -> Just (Num (max 0 (n-1)))
        _                   -> Nothing

bigStep (ZeroP e) = 
    case bigStep e of
        Just (Num n)    -> Just (Boolean (n == 0))
        _               -> Nothing

bigStep (Let bindings cuerpo) =
    case evaluarBindings bindings of
        Just bindingsFinales -> bigStep (sustMany cuerpo bindingsFinales)
        Nothing              -> Nothing

bigStep (LetStar [] cuerpo) = bigStep cuerpo

bigStep (LetStar ((x, e1) : resto) cuerpo) =
    case bigStep e1 of
        Just valorFinal -> bigStep (sust (LetStar resto cuerpo) x valorFinal)
        Nothing         -> Nothing


evaluarNumeros :: [ASA] -> Maybe [Int]
evaluarNumeros [] = Just []
evaluarNumeros (n1:resto) =
    case (bigStep n1, evaluarNumeros resto) of
        (Just (Num n), Just listaNum)       -> Just (n : listaNum)
        _                                   -> Nothing 

evaluarBooleanos :: [ASA] -> Maybe [Bool]
evaluarBooleanos [] = Just []
evaluarBooleanos (b1:resto) = 
    case (bigStep b1, evaluarBooleanos resto) of
        (Just (Boolean b), Just booleanos)  -> Just (b : booleanos)
        _                                   -> Nothing 

verificarRelacion :: (Int -> Int -> Bool) -> [Int] -> Bool
verificarRelacion _ [] = True
verificarRelacion _ [_] = True
verificarRelacion op (n1:n2:xs) = op n1 n2 && verificarRelacion op (n2:xs)

evaluarBindings :: [Binding] -> Maybe [Binding]
evaluarBindings [] = Just []
evaluarBindings ((var, e) : resto) =
    case (bigStep e, evaluarBindings resto) of
        (Just valorEvaluado, Just bindingsEvaluados) -> Just ((var, valorEvaluado) : bindingsEvaluados)
        _                                            -> Nothing