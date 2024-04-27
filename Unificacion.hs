module Unificacion where

import Data.List

--Tipo de datos Nombre para representar las cadenas.
type Nombre = String

-- Tipo de datos para representar los términos como una variable
-- o un símbolo de función seguido de una lista de términos.
data Termino = V Nombre
               | T Nombre [Termino]
               deriving Eq

instance Show Termino where
    show (V nombre) = nombre
    show (T nombre []) = nombre
    show (T nombre ts) = nombre ++ concat [show ts] 

-- Devuelve el nombre de un término
nombreTermino :: Termino -> Nombre
nombreTermino (V nombre) = nombre
nombreTermino (T nombre _) = nombre

-- Tipo de datos Atomo para representar los átomos como un símbolo
-- de predicado seguido de una lista de términos, o bien una igualdad.
data Atomo = A Nombre [Termino]
             | I Termino Termino
             deriving Eq

-- Función que verifica si un átomo es una identidad
esIdentidad :: Atomo -> Bool
esIdentidad (I _ _) = True
esIdentidad _ = False

instance Show Atomo where
    show (A nombre []) = nombre
    show (A nombre ts) = nombre ++ concat [show ts]
    show (I t1 t2) = (show t1) ++ " = " ++ (show t2)

-- Devuelve el nombre de un átomo
nombreAtomo :: Atomo -> Nombre
nombreAtomo (A nombre _) = nombre
nombreAtomo (I _ _) = ""

-- Tipo de datos Variable
type Variable = Termino

-- Función que verifica si un término es una variable
esVariable :: Termino -> Bool
esVariable (V _) = True
esVariable _ = False

-- Función que devuelve una lista con todas las variables de un término
-- sin repeticiones
variables :: Termino -> [Variable]
variables (V v) = [V v]
variables (T n ts) = variablesEnLista ts

-- Función que devuelve ua lista con todas las variables de una lista de términos
-- sin repeticiones
variablesEnLista :: [Termino] -> [Variable]
variablesEnLista = nub . concat . map variables

-- Tipo de datos Sustitucion para representar las listas de pares formados
-- por una variable y un término
type Sustitucion = [(Variable,Termino)]

-- La sustitución identidad
epsilon :: Sustitucion
epsilon = []

-- Función que devuelve el dominio de una sustitución
dominio :: Sustitucion -> [Variable]
dominio = map fst

-- Función que aplica una sustitución sobre una variable
aplicaVar :: Sustitucion -> Variable -> Termino
aplicaVar [] (V y) = V y
aplicaVar ((x,t):xs) (V y)
    | x == (V y) = t
    | otherwise = aplicaVar xs (V y)

-- Función que aplica una sustitución sobre las variables de un término
aplicaT :: Sustitucion -> Termino -> Termino
aplicaT s (V x) = aplicaVar s (V x)
aplicaT s (T n ts) = T n [aplicaT s t | t <- ts]

--Función que reduce una sustitución eliminando los pares cuyos elementos
-- son iguales
reduce :: Sustitucion -> Sustitucion
reduce s = [(x,t) | (x,t) <- s, x/=t]

-- Función que compone dos sustituciones en una sola
composicion :: Sustitucion -> Sustitucion -> Sustitucion
composicion xs ys =
    (reduce [(y,aplicaT ys t) | (y,t) <- xs])
    ++
    [(x,t) | (x,t) <- ys, x `notElem` (dominio xs)]

-- Función que devuelve un unificador de máxima generalidad de dos términos, en caso
-- de ser unificables, y la lista vacía en caso contrario
unifica :: Termino -> Termino -> [Sustitucion]
unifica (V x) (V y)
    | x==y = [epsilon]
    | otherwise = [[(V x, V y)]]
unifica (V x) t2 = [[(V x,t2)] | (V x) `notElem` variables t2]
unifica t1 (V y) = [[(V y,t1)] | (V y) `notElem` variables t1]
unifica (T f ts) (T g rs) = [u | f == g, u <- unificaListas ts rs]

-- Función que devuelve el unificador de máxima generalidad entre
-- dos listas de términos
unificaListas :: [Termino] -> [Termino] -> [Sustitucion]
unificaListas [] [] = [epsilon]
unificaListas [] (r:rs) = []
unificaListas (t:ts) [] = []
unificaListas (t:ts) (r:rs) =
    [composicion sigma2 sigma1
        | sigma1 <- unifica t r,
          sigma2 <- unificaListas [aplicaT sigma1 t | t <- ts]
                                  [aplicaT sigma1 r | r <- rs]]

-- Función que devuelve el unificador de máxima generalidad de dos átomos 
-- si son unificables y la lista vacía en otro caso
unificaA :: Atomo -> Atomo -> [Sustitucion]
unificaA (A n1 ts1) (A n2 ts2)
    | n1 == n2 = unificaListas ts1 ts2
    | otherwise = []
unificaA (A n ts) (I t1 t2) = []
unificaA (I t1 t2) (A n ts) = []
unificaA (I t1 t2) (I t3 t4) = []

-- Devuelve el unificador de máxima generalidad que resuelve una identidad
-- Si la entrada no es una identidad, devuelve la sustitución vacía
unificaIdentidad :: Atomo -> [Sustitucion]
unificaIdentidad (I t1 t2) = unifica t1 t2
unificaIdentidad _ = []