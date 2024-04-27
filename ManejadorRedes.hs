module ManejadorRedes (Red(..),Nodo,ClausulaRed(..),Origen,creaNodoVal,creaNodo,universal,creaOrigen,
    origenUniversal,creaClausulaRed,fusiona,
    devuelveRestrC,devuelveRestriccionesC,esConsistenteCR,getNodosCR,
    getTiempoDFLT,getTiempoBorrosoDFLT,getTiempo,getTiempoBorroso) where

import FCN
import NumerosBorrosos
import Data.List
import Data.Time 
import qualified Data.Map.Lazy as Map
import ManejoTiempo

type Nombre = String
type Valor = String

-- Un nodo es una variable con un valor, ambos cadenas
data Nodo = N Nombre Valor

instance Eq Nodo where
    (==) = eqNodo

instance Ord Nodo where
    (<=) = leqNodo

instance Show Nodo where
    show = showNodo

getNombre :: Nodo -> Nombre
getNombre (N nom _) = nom

getValor :: Nodo -> Valor
getValor (N _ val) = val

--type Mapa = [(Nodo,Int)] 
type Mapa = Map.Map Nodo Int 

data Origen = O Nodo ZonedTimeBorroso

instance Eq Origen where
    (==) = esIgualOr

-- Una cláusula red incluye las restricciones entre hechos
data ClausulaRed = CR Origen Mapa Red

instance Show ClausulaRed where
    show = showClausulaRed

instance Eq ClausulaRed where
    (==) = esIgual


-- Crea un nodo con valor asignado
creaNodoVal :: Nombre -> Valor -> Nodo
creaNodoVal nombre valor = N nombre valor

-- Crea un nodo sin valor asignado
creaNodo :: Nombre -> Nodo
creaNodo nombre = creaNodoVal nombre ""

-- Comprueba si un nodo tiene valor o no
esVacio :: Nodo -> Bool
esVacio (N _ val) = val == ""

-- Origen universal: ningún nodo, a las 0:00 del 1 de enero del año 1
origenUniversal :: Origen
origenUniversal = O (creaNodo "") tiempo0borroso

-- Red universal
universal :: ClausulaRed
universal = creaClausulaRed origenUniversal []

-- Crea un origen
creaOrigen :: Nodo -> (String,String,String,String) -> Origen
creaOrigen nodo tiempos = O nodo (creaTiempoBorroso tiempos)

-- Crea una clausula de red
creaClausulaRed :: Origen -> [(Nodo,Nodo,NumeroBorroso,UnidadTiempo)] -> ClausulaRed
creaClausulaRed (O o ztb) rs = CR (O newo ztb) mapa (minimizaRed (creaRed (traduceTuplas mapa restrcs)))
    where
        newo = traduceNodoR mapa (traduceNodo mapa o);
        mapa = mapeaNodos restrcs;
        restrcs = [(n1,n2,escala nb (segundos unidad)) | (n1,n2,nb,unidad) <- rs]

-- Crea un mapeo de los nodos de la red
mapeaNodos :: [(Nodo,Nodo,NumeroBorroso)] -> Mapa
mapeaNodos cr = mapeaNodosAux Map.empty (nodos cr)

mapeaNodosAux :: Mapa -> [Nodo] -> Mapa
mapeaNodosAux m [] = m
mapeaNodosAux m (n:nodos) = mapeaNodosAux (mezclaNodo m n) nodos

-- Obtiene todos los nodos
nodos :: [(Nodo,Nodo,NumeroBorroso)] -> [Nodo]
nodos cr = [n1 | (n1,n2,num) <- cr] ++ [n2 | (n1,n2,num) <- cr]

-- Traduce los nodos en una clausula por su correspondiente número
traduceTuplas :: Mapa -> [(Nodo,Nodo,NumeroBorroso)] -> [(Int,Int,NumeroBorroso)]
traduceTuplas ms cr = [(traduceNodo ms n1, traduceNodo ms n2, num) | (n1,n2,num) <- cr]

-- Devuelve el número asignado a un nodo
{-traduceNodo :: Mapa -> Nodo -> Int
traduceNodo ms nodo = traduceNodoAux ms nodo 0

traduceNodoAux :: Mapa -> Nodo -> Int -> Int
traduceNodoAux [] nodo k = k
traduceNodoAux ((s,n):ms) nodo k
    | nodo == s = n
    | (nombre1 == nombre2) && (esVacio nodo || esVacio s) = n
    | otherwise = traduceNodoAux ms nodo (k+1)
        where
            nombre1 = getNombre nodo;
            nombre2 = getNombre s;-}

-- Devuelve todas las posibles respuestas de número asignado a un nodo
{-traduceNodo :: Mapa -> Nodo -> [Int]
traduceNodo ms nodo 
    | (esVacio nodo) && (Map.notMember nodo ms) = def
    | otherwise = traduceNodoAux ms nodo def
    where
        def = Map.size ms 

traduceNodoAux :: Mapa -> Nodo -> Int -> [Int]
traduceNodoAux ms nodo def = 
    | (esVacio nodo) && (Map.notMember nodo ms) = []
    | (esVacio nodo) && (Map.member nodo ms) = find:(traducenodoAux newms nodo def)
    | otherwise = [find] 
    where
        find = Map.findWithDefault def nodo ms;
        newms = Map.delete nodo ms
-}

-- Devuelve el número asignado a un nodo
traduceNodo :: Mapa -> Nodo -> Int
traduceNodo ms nodo =  Map.findWithDefault def nodo ms
    where
        def = Map.size ms 

-- Fusiona la segunda cláusula a la primera
fusiona :: ClausulaRed -> ClausulaRed -> ClausulaRed
fusiona (CR o1 m1 r1) (CR o2 m2 r2) = CR (O newo ztb) mapa (minimizaRed (mezclaRed r1 r2 c))
    where
        newo = traduceNodoR mapa (traduceNodo mapa o);
        (O o ztb) = if o1 == origenUniversal then o2 else o1
        mapa = mezclaMapa m1 m2 ;
        c = mapeaRedes (CR o2 m2 r2) (CR o1 m1 r1)


-- Mezcla la información de un nodo a un mapa
mezclaNodo :: Mapa -> Nodo -> Mapa
mezclaNodo m nodo 
    | esVacio nodo && n < tam = m
    | not (esVacio nodo) && n < tam = Map.insert nodo n (Map.delete nodo m)
    | otherwise = Map.insert nodo tam m
    where
        tam = Map.size m;
        n = traduceNodo m nodo

-- Añade la información del segundo mapa al primero, traduciéndola por el camino
mezclaMapa :: Mapa -> Mapa -> Mapa
mezclaMapa m1 m2 
    | Map.null m1 = m2
    | Map.null m2 = m1
    | otherwise = mezclaMapa (mezclaNodo m1 s) (Map.delete s m2)
    where
        s = head (Map.keys m2)

-- Crea un mapa entre los nodos de las redes en las cláusulas
mapeaRedes :: ClausulaRed -> ClausulaRed -> [(Int,Int)]
mapeaRedes (CR _ m1 r1) (CR _ m2 r2) = [(traduceNodo m1 n, traduceNodo m11 n) | n <- Map.keys m1]
    where
        m11 = mezclaMapa m2 m1

-- Devuelve la restricción entre 2 nodos, en segundos
devuelveRestrC :: ClausulaRed -> Nodo -> Nodo -> NumeroBorroso
devuelveRestrC (CR _ m r) a b = FCN.devuelveRestr r n1 n2
    where
        n1 = traduceNodo m a ;
        n2 = traduceNodo m b

-- Devuelve las restricciones borrosas entre los nodos, en segundos
devuelveRestriccionesC :: ClausulaRed -> [Nodo] -> [(Nodo,Nodo,NumeroBorroso)]
devuelveRestriccionesC cr ns = devuelveRestriccionesC' cr (pares ns)

devuelveRestriccionesC' :: ClausulaRed -> [(Nodo,Nodo)] -> [(Nodo,Nodo,NumeroBorroso)]
devuelveRestriccionesC' cr ns = [(s1,s2,devuelveRestrC cr s1 s2) | (s1,s2) <- ns]

-- Devuelve una lista con los nodos, en el orden en que fueron
-- introducidos en la cláusula de red
getNodosCR :: ClausulaRed -> [Nodo]
getNodosCR (CR _ m r) = [traduceNodoR m n | n <- [0..(numNodos r)]]

-- Indica si una red es consistente o no
esConsistenteCR :: ClausulaRed -> Bool
esConsistenteCR (CR _ m r) = esRed r

-- Devuelve el tiempo borroso asignado a un nodo, en base al nodo origen por defecto
getTiempoBorrosoDFLT :: ClausulaRed -> Nodo -> ZonedTimeBorroso
getTiempoBorrosoDFLT (CR (O n ztb) m r) nodo = getTiempoBorroso (CR (O n ztb) m r) nodo n (defuzzyTime ztb)

-- Devuelve el tiempo asignado a un nodo, en base al nodo origen por defecto
getTiempoDFLT :: ClausulaRed -> Nodo -> ZonedTime
getTiempoDFLT (CR (O n ztb) m r) nodo = getTiempo (CR (O n ztb) m r) nodo n (defuzzyTime ztb)

-- Devuelve el tiempo borroso asignado a un nodo, dados el nodo origen, su tiempo y las unidades
-- de las restricciones
getTiempoBorroso :: ClausulaRed -> Nodo -> Nodo -> ZonedTime -> ZonedTimeBorroso
getTiempoBorroso cr nodo origen zt = sumaTiempoBorroso zt diff Segundo
    where
        diff = devuelveRestrC cr origen nodo

-- Devuelve el tiempo defuzzificado asignado a un nodo, dados el nodo origen, su tiempo y las unidades
-- de las restricciones
getTiempo :: ClausulaRed -> Nodo -> Nodo -> ZonedTime-> ZonedTime
getTiempo cr nodo origen zt = sumaTiempo zt diff Segundo
    where
        diff = defuzzy (devuelveRestrC cr origen nodo)
-------------------------------------------------------------------------
eqNodo :: Nodo -> Nodo -> Bool
eqNodo (N n1 v1) (N n2 v2) = n1 == n2 && (v1 == v2 || v1 == "" || v2 == "")

leqNodo :: Nodo -> Nodo -> Bool
leqNodo (N n1 v1) (N n2 v2) 
    | n1 == n2 = (v1 <= v2 || v1 == "" || v2 == "")
    | otherwise = n1 < n2

showNodo :: Nodo -> String
showNodo (N nom val) 
    | val == "" = nom
    | otherwise = nom ++ "=" ++ val

esIgualOr :: Origen -> Origen -> Bool
esIgualOr (O n1 ztb1) (O n2 ztb2) = n1==n2 && (zonedTimeBorrosoToUTCBorroso ztb1 == zonedTimeBorrosoToUTCBorroso ztb2)

showClausulaRed :: ClausulaRed -> String
showClausulaRed (CR o m r)
    | (CR o m r) == universal = "universal"
    | otherwise = concat [(muestra (traduceNodoR m n1,traduceNodoR m n2,devuelveRestr r n1 n2))++"\n" | n1 <- [0..n], n2 <- [n1..n]]
    where
        n = ((Map.size m) - 1);
        muestra (nodo1,nodo2,p) = if p == infinito then "(" ++ (show nodo1) ++ "," ++ (show nodo2) ++ ",infinite)" 
                                    else "(" ++ (show nodo1) ++ "," ++ (show nodo2) ++ "," ++ (show (creaCantidadTiempoBorroso p Segundo)) ++ ")"

-- Devuelve el nodo asociado a un entero
traduceNodoR :: Mapa -> Int -> Nodo
traduceNodoR m k 
    | Map.null m = creaNodo ""
    | k == n = s
    | otherwise = traduceNodoR (Map.delete s m) k
    where
        (s,n) = head (Map.toList m)


-- TODO: Revisar
esIgual :: ClausulaRed -> ClausulaRed -> Bool
esIgual (CR o1 m1 r1) (CR o2 m2 r2) = (o1 == o2) && (nodos1 == nodos2) && 
    and [devuelveRestrC (CR o1 m1 r1) s1 s2 == devuelveRestrC (CR o2 m2 r2) s1 s2 | (s1,s2) <- (pares nodos1)]
    where
        nodos1 = sort [n | (n,i) <- Map.toList m1] ;
        nodos2 = sort [n | (n,i) <- Map.toList m2]

pares :: [a] -> [(a,a)]
pares xs = paresAux xs xs

paresAux :: [a] -> [b] -> [(a,b)]
paresAux [] _ = []
paresAux _ [] = []
paresAux (x:xs) ys = [(x,y) | y <- ys] ++ (paresAux xs (tail ys))  
