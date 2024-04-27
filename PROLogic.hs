module PROLogic where

import Unificacion
import Prolog 
import ManejadorRedes
import Data.List
import NumerosBorrosos
import Data.Time
import ManejoTiempo

-- Cláusulas de PROLogic, con la parte izquierda una cláusula de prolog y la
-- parte derecha una red
type ClausulaAmp = (Clausula,ClausulaRed)

-- Un programa en PROLogic será una lista de las cláusulas ampliadas
type ProgramaAmp = [ClausulaAmp]

-- Un objetivo puede incluir un patrón temporal
type ObjetivoAmp = (Objetivo,ClausulaRed)

-- Las respuestas incluirán una red
type Respuesta = (Sustitucion,ClausulaRed)

---------------------------------CREACION---------------------------------------------
programaVacio :: ProgramaAmp
programaVacio = creaProgramaAmp []

hechoTime :: ClausulaAmp
hechoTime = ((A "time" [],[]),universal)

atomoTime :: Atomo
atomoTime = A "time" []

creaClausulaAmp :: Clausula -> ClausulaRed -> ClausulaAmp
creaClausulaAmp c cr = (c,cr)
   -- | null (cuerpo c) = (c,cr)
   -- | otherwise = ((cabeza c,cuerpo c++[atomoTime]),cr) 

creaProgramaAmp :: [ClausulaAmp] -> ProgramaAmp
creaProgramaAmp cs = cs
--    | not (contieneTime cs) = cs++[hechoTime]
--    | otherwise = cs

contieneTime :: ProgramaAmp -> Bool
contieneTime [] = False
contieneTime ((c,cr):cs) = (nombreAtomo (cabeza c) == "time") || contieneTime cs

creaObjetivoAmp :: Objetivo -> ClausulaRed -> ObjetivoAmp
--creaObjetivoAmp o cr = (o++[atomoTime],cr)
creaObjetivoAmp o cr = (o,cr)

---------------------------------CONSULTAS---------------------------------------------

-- Función que devuelve una lista de posibles respuestas a un objetivo dado un programa
respuestasAmp :: ProgramaAmp -> ObjetivoAmp -> [Respuesta]
respuestasAmp p o = respuestasAmp' p o 0 []

respuestasAmp' :: ProgramaAmp -> ObjetivoAmp -> Nivel -> Sustitucion -> [Respuesta]
respuestasAmp' p (o,cr1) n s
    | not (esConsistenteCR cr1) = []
    | null o = [(s,cr1)]
    | esIdentidad obj = if not (null us) then 
                            respuestasAmp' p (aplica u (tail o), cr1) (n+1) (composicion s u)
                        else 
                            []
    | otherwise = concat [respuestasAmp' p
                                          (aplica u (resolvente o c n), fusiona cr1 cr2) 
                                          (n+1)
                                          (composicion s u)
                          | (c,cr2) <- p
                          , let us = unificaA (renombraA (cabeza c) n)
                                              (head o)
                          , not (null us)
                          , let u = head us]
    where
        obj = head o;
        us = unificaIdentidad obj;
        u = head us

-- Función que obtiene una lista de posibles respuestas a una consulta en un programa
respuestasReducidasAmp :: ProgramaAmp -> ObjetivoAmp -> [Respuesta]
respuestasReducidasAmp p (o,cr) = [(calculaRespuesta var s,cr) 
                                   | (s,cr) <- res]
    where
        var = variablesObjetivo o ;
        res = respuestasAmp p (o,cr)


-- Función que devuelve los nodos sucesores a un indicado, en la red de una respuesta
getSucesores :: Respuesta -> Nodo -> [Nodo]
getSucesores (s,cr) nodo = [n | n <- nodos, posterior n nodo, n /= nodo]
    where
      nodos = getNodosCR cr;
      posterior n nodo = (defuzzy (devuelveRestrC cr nodo n)) >= 0

-- Función que devuelve los nodos predecesores a un indicado, en la red de una respuesta
getPredecesores :: Respuesta -> Nodo -> [Nodo]
getPredecesores (s,cr) nodo = [n | n <- nodos, anterior n nodo, n /= nodo]
    where
      nodos = getNodosCR cr;
      anterior n nodo = (defuzzy (devuelveRestrC cr nodo n)) <= 0

-- Función que devuelve los nodos que se corresponden a eventos iniciales de una red
getPrimeros :: Respuesta -> [Nodo]
getPrimeros (s,cr) = filter esPrimero nodos
    where
      nodos = getNodosCR cr;
      esPrimero n = and [defuzzy (devuelveRestrC cr p n) == 0 | p <- getPredecesores (s,cr) n] 

-- Función que devuelve los nodos que se corresponden a eventos finales de una red
getFinales :: Respuesta -> [Nodo]
getFinales (s,cr) = filter esFinal nodos
    where
      nodos = getNodosCR cr;
      esFinal n = and [defuzzy (devuelveRestrC cr n s) == 0 | s <- getSucesores (s,cr) n] 

-- Función que devuelve el tiempo de un nodo en la red de una respuesta
getTiempoPL :: Respuesta -> Nodo -> ZonedTime
getTiempoPL (s,cr) nodo = getTiempoDFLT cr nodo

-- Función que devuelve el tiempo borroso de un nodo en la red de una respuesta
getTiempoBorrosoPL :: Respuesta -> Nodo -> ZonedTimeBorroso
getTiempoBorrosoPL (s,cr) nodo = getTiempoBorrosoDFLT cr nodo

-- Función que devuelve la restricción relativa entre 2 nodos, en segundos
getRestriccion :: Respuesta -> Nodo -> Nodo -> NumeroBorroso
getRestriccion (s,cr) n1 n2 = devuelveRestrC cr n1 n2

----------------------------------ESCRITURA-----------------------------------------
-- Función que escribe una respuesta junto a su red completa
-- Los booleanos son flags: (MuestraRed,Defuzzyficado,MuestraInfinitos)
escribeRespuestaCompleta :: Respuesta -> (Bool,Bool,Bool) -> IO()
escribeRespuestaCompleta (s,cr) opt =
  do 
    let nodos = getNodosCR cr
    escribeRespuestaParcial (s,cr) nodos opt

-- Función que escribe una respuesta junto a su red parcial, mostrando
-- las relaciones entre los nodos indicados
-- Los booleanos son flags: (MuestraRed,Defuzzyficado,MuestraInfinitos)
escribeRespuestaParcial :: Respuesta -> [Nodo] -> (Bool,Bool,Bool) -> IO()
escribeRespuestaParcial (s,cr) nodos (red,def,inf) =
  do 
    if null s then putStrLn ("Yes.\n") 
    else escribeSustitucion s
    if red then escribeRed (s,cr) nodos (def,inf)
      else putStr ""
        
-- Función que escribe las restricciones entre los nodos indicados de una respuesta
escribeRed :: Respuesta -> [Nodo] -> (Bool,Bool) -> IO()
escribeRed (s,(CR o m (R i a (po,ne)))) nodos (def,inf) =
  do
    --putStr "Degrees of possibility: "
    --putStrLn (show po)                        
    -- putStr "Degrees of necessity: "             
    -- putStrLn (show ne)                          
    putStr "Certainty: "
    -- putStrLn (show (sum  ne/(fromIntegral.length) ne)) -- media    
    putStrLn (show (maximum ne)) -- máximo
    putStrLn "\nTemporal constraints:"
    if def then
        putStrLn (concat [(muestra (n1,n2,defuzzy nb)) | (n1,n2,nb) <- devuelveRestriccionesC cr nodos, nb/=infinito || inf])
      else
        putStrLn (concat [(muestra' (n1,n2,nb)) | (n1,n2,nb) <- devuelveRestriccionesC cr nodos, nb/=infinito || inf])
      where
        muestra (n1,n2,dn) = "(" ++ (show n1) ++ "," ++ (show n2) ++ "," ++ (show (creaCantidadTiempo dn Segundo)) ++ ")\n";
        muestra' (n1,n2,nb) = "(" ++ (show n1) ++ "," ++ (show n2) ++ "," ++ (show (creaCantidadTiempoBorroso nb Segundo)) ++ ")\n"
        cr = CR o m (R i a (po,ne))

-- Función que escribe la solución de una red desde su nodo origen por defecto
escribeSolucionRed :: Respuesta -> Bool -> [Nodo] -> IO()
escribeSolucionRed (s,cr) def listNodos =
    do
      let nodos = if null listNodos then getNodosCR cr 
                    else listNodos
      if def then
          putStrLn (concat [(show n)++" -> "++showTiempo (getTiempoDFLT cr n) ++ "\n" | n <- nodos])
       else
          putStrLn (concat [(show n)++" -> "++showTiempoBorroso (getTiempoBorrosoDFLT cr n) ++ "\n" | n <- nodos])

-- Función que escribe la solución a una red introduciendo un nodo de origen
escribeSolucionRedCustom :: Respuesta -> Bool -> Nodo -> ZonedTime -> [Nodo] -> IO()
escribeSolucionRedCustom (s,cr) def origen tiempo listNodos =
    do
      let nodos = if null listNodos then getNodosCR cr 
                    else listNodos
      if not (origen `elem` nodos) then putStrLn ("The node does not exist " ++ (show origen) ++ ".")
        else 
          do
            putStrLn "Network times:"
            putStrLn ((show origen) ++ " -> " ++ (show tiempo))
            if def then 
               putStrLn (concat [(show n) ++ " -> " ++ (showTiempo (getTiempo cr n origen tiempo)) ++ "\n" | n <- nodos, n /= origen])
            else
               putStrLn (concat [(show n) ++ " -> " ++ (showTiempoBorroso (getTiempoBorroso cr n origen tiempo)) ++ "\n" | n <- nodos, n /= origen])


-- Función que escribe una respuesta con todas las posibles sustituciones
escribeRespuestas :: [Respuesta] -> IO()
escribeRespuestas rs = escribeRespuestas' (zip [1..] rs)

escribeRespuestas' :: [(Int,Respuesta)] -> IO()
escribeRespuestas' [] = putStrLn "There are no more answers."
escribeRespuestas' ((n,(s,cr)):nrs) = 
    do putStrLn ("Answer " ++ (show n) ++ ":")
       escribeRespuestaCompleta (s,cr) (True,False,False)
       putStrLn ""
       escribeRespuestas' nrs

-- Función de consulta en PROLogic
prologic :: ProgramaAmp -> ObjetivoAmp -> IO()
prologic p o 
    | null res = putStrLn "There is no answer."
    | otherwise = escribeRespuestas res
    where res = respuestasReducidasAmp p o