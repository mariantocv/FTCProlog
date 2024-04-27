-- AUTOR: José Antonio Alonso Jiménez
-- Esta obra está bajo una licencia Reconocimiento–NoComercial–CompartirIgual 2.5 Spain de Creative Commons.
module Prolog where

import Unificacion
import Data.List
import Debug.Trace

-- Tipo de datos Cláusula para representar las cláusula como pares formados
-- por un átomo y una lista de átomos
type Clausula = (Atomo,[Atomo])

-- Tipo de datos Objetivo para representar los objetivos
-- como listas de átomos
type Objetivo = [Atomo]

-- Tipo de datos Programa para representar los programas como listas
-- de cláusulas
type Programa = [Clausula]

-- Tipo de datos Nivel como sinónimo de números enteros
type Nivel = Int

-- Función que devuelve la cabeza de una cláusula
cabeza :: Clausula -> Atomo
cabeza = fst

-- Función que devuelve el cuerpo de una cláusula
cuerpo :: Clausula -> [Atomo]
cuerpo = snd

-- Función que renombra las variables de un término poniéndoles un subíndice
renombraT :: Termino -> Nivel -> Termino
renombraT (V x) n = V (x ++ "_" ++ show n)
renombraT (T f ts) n = T f [aplicaT s t | t <- ts]
                        where s = [(x,renombraT x n) | x <- variablesEnLista ts]

-- Función que renombra las variables de un átomo poniéndoles un subíndice
renombraA :: Atomo -> Nivel -> Atomo
renombraA (A p ts) n = A p [renombraT t n | t <- ts]
renombraA (I t1 t2) n = I (renombraT t1 n) (renombraT t2 n)

-- Función que renombra las variables de una lista de átomos
renombra :: [Atomo] -> Nivel -> [Atomo]
renombra as n = [renombraA a n | a <- as]

-- Función que dada una cláusula y un objetivo, devuelve el resolvente
resolvente :: Objetivo -> Clausula -> Nivel -> Objetivo
resolvente o c n = (renombra (cuerpo c) n) ++ (tail o)

-- Función que aplica una sustitución a un átomo
aplicaA :: Sustitucion -> Atomo -> Atomo
aplicaA s (A n ts) = A n [aplicaT s t | t <- ts]
aplicaA s (I t1 t2) = I (aplicaT s t1) (aplicaT s t2)

-- Función que aplica una sustitución a un objetivo
aplica :: Sustitucion -> Objetivo -> Objetivo
aplica s o = [aplicaA s a | a <- o]

-- Función que devuelve una lista de posibles respuestas a un objetivo dado un programa
respuestas :: Programa -> Objetivo -> [Sustitucion]
respuestas p o = respuestas' p o 0 []

respuestas' :: Programa -> Objetivo -> Nivel -> Sustitucion -> [Sustitucion]
respuestas' p o n s
    | null o = [s]
    | esIdentidad obj = if not (null us) then 
                              respuestas' p (aplica u (tail o)) (n+1) (composicion s u)
                           else 
                              []
    | otherwise = concat [respuestas' p
                                      (aplica u (resolvente o c n))
                                      (n+1)
                                      (composicion s u)
                          | c <- p
                          , let us = unificaA (renombraA (cabeza c) n)
                                              (head o)
                          , not (null us)
                          , let u = head us]
    where
          obj = head o;
          us = unificaIdentidad obj;
          u = head us

-- Función que une los conjuntos de una lista de conjuntos
unionGeneral :: Eq a => [[a]] -> [a]
unionGeneral [] = []
unionGeneral (x:xs) = x `union` unionGeneral xs

-- Función que devuelve la lista de variables de un átomo
variablesAtomo :: Atomo -> [Variable]
variablesAtomo (A _ ts) = variablesEnLista ts
variablesAtomo (I t1 t2) = variablesEnLista [t1,t2]

-- Función que devuelve la lista de variables de un objetivo
variablesObjetivo :: Objetivo -> [Variable]
variablesObjetivo o = unionGeneral [variablesAtomo a | a <- o]

-- Función que evalúa un término dada una sustitución
valor :: Termino -> Sustitucion -> Termino
valor (V x) s 
    | elem (V x) (dominio s) = valor (head [t | (y,t) <- s, y==V x]) s
    | otherwise = V x
valor (T f ts) s = T f [valor t s | t <- ts]

-- Función que dada una lista de variables y una sustitucion, establece
-- los valores de las variables en forma de sustitucion
calculaRespuesta :: [Variable] -> Sustitucion -> Sustitucion
calculaRespuesta xs s = [(x,valor x s) | x <- xs]

-- Función que obtiene una lista de posibles respuestas a una consulta en un programa
respuestasReducidas :: Programa -> Objetivo -> [Sustitucion]
respuestasReducidas p o = [calculaRespuesta var s | s <- res]
    where
        var = variablesObjetivo o ;
        res = respuestas p o

-- Función que escribe una asignación a una variable 
escribeLigadura :: (Variable,Termino) -> IO()
escribeLigadura (x,t) = putStrLn ((show x)++" = "++(show t))

--Función que escribe una sustitución completa
escribeSustitucion :: Sustitucion -> IO()
escribeSustitucion [] = putStrLn ""
escribeSustitucion (l:ls) = do escribeLigadura l
                               escribeSustitucion ls

-- Función que escribe una respuesta con todas las posibles sustituciones
escribeRespuesta :: [Sustitucion] -> IO()
escribeRespuesta rs = escribeRespuesta' (zip [1..] rs)

escribeRespuesta' :: [(Int,Sustitucion)] -> IO()
escribeRespuesta' [] = putStrLn "No hay más respuestas."
escribeRespuesta' ((n,r):nrs) = 
    do putStrLn ("Respuesta " ++ (show n) ++ ":")
       escribeSustitucion r
       escribeRespuesta' nrs

-- Función de consulta en Prolog
prolog :: Programa -> Objetivo -> IO()
prolog p o
    | null res = putStrLn "No hay respuesta."
    | otherwise = escribeRespuesta res
    where res = respuestasReducidas p o