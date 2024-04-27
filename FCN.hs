module FCN (Red(..), creaRed, devuelveRestr, mezclaRed, minimizaRed, esRed, numNodos) where

import NumerosBorrosos

-- Una restricción es un número borroso
type Restr = NumeroBorroso

-- NUEVO: Se ha añadido a la definición de Red una lista con medidas de posibilidad y de necesidad
type Posibilidad = [Double]
type Necesidad = [Double]
type Grados = (Posibilidad,Necesidad)

-- Restricción borrosa entre 2 nodos
data Arco = Arc {nodo1::Int, nodo2::Int, restr::Restr}
    deriving (Show, Eq) 

-- Red de restricciones borrosas
data Red = R Int [Arco] Grados | NoRed 

instance Eq Red where
    (==) = esIgual

instance Show Red where
    show = showRed

-- Una correspondencia entre nodos de 2 redes
type Corresp = [(Int,Int)]


-- Función que crea una red con arcos en un sólo sentido
creaRed :: [(Int,Int,Restr)] -> Red
creaRed [] = (R 0 [] ([1],[0]))
creaRed ((n1,n2,r):rs) = mezclaRestr (creaRed rs) n1 n2 r 

-- Función que dada una red y 2 etiquetas de nodo, devuelve la restricción
-- entre esos 2 nodos
devuelveRestr :: Red -> Int -> Int -> Restr
devuelveRestr NoRed _ _ = infinito
devuelveRestr (R n rs g) a b 
    | not (null restrs) = head restrs
    | not (null restrsInv) = head restrsInv
    | otherwise = infinito
        where 
            restrs = [r | (Arc n1 n2 r) <- rs, n1==a, n2==b] ;
            restrsInv = [opuestoBorroso r | (Arc n1 n2 r) <- rs, n1==b, n2==a]

-- Obtiene la red mínima equivalente a la red dada
minimizaRed :: Red -> Red
minimizaRed NoRed = NoRed 
minimizaRed (R n rs g) = minimizaRedAux (R n rs g) n

minimizaRedAux :: Red -> Int -> Red
minimizaRedAux NoRed _ = NoRed
minimizaRedAux (R n rs g) k 
    | k <= (-1) = (R n rs g)
    | otherwise = minimizaRedAux (minimizaRedAux2 (R n rs g) k (todosArcos (R n rs g))) (k-1)

minimizaRedAux2 :: Red -> Int -> [Arco] -> Red
minimizaRedAux2 NoRed _ _ = NoRed
minimizaRedAux2 (R n rs g) k [] = (R n rs g)
minimizaRedAux2 (R n rs g) k (r:rs2) = minimizaRedAux2 (minimizaRestr (R n rs g) k i j) k rs2
    where i = nodo1 r ;
          j = nodo2 r

-- Coge la restriccion minima entre 2 nodos y el camino entre ellos pasando por un tercero
minimizaRestr :: Red -> Int -> Int -> Int -> Red
minimizaRestr NoRed _ _ _ = NoRed
minimizaRestr (R n rs g) k i j = 
    actualizaRestr (R n rs g) i j (interseccionBorrosa (devuelveRestr (R n rs g) i j)
                        (sumaBorrosa (devuelveRestr (R n rs g) i k) (devuelveRestr (R n rs g) k j)))

actualizaRestr :: Red -> Int -> Int -> Restr -> Red
actualizaRestr NoRed _ _ _ = NoRed 
actualizaRestr (R n rs g) i j r
    | esConsistente r = R n ((Arc i j r):resto) g
    | otherwise = NoRed 
        where
            resto = [Arc n1 n2 p | (Arc n1 n2 p) <- rs, not ((n1==i && n2==j) || (n1==j && n2==i))]

-- Devuelve todos los arcos de la red en un único sentido.
todosArcos :: Red -> [Arco]
todosArcos NoRed = []
todosArcos (R n rs g) = [Arc n1 n2 (devuelveRestr (R n rs g) n1 n2) | n1 <- [0..n], n2 <- [n1..n]]

-- Mezcla 2 redes
mezclaRed :: Red -> Red -> Corresp -> Red
mezclaRed NoRed _ _ = NoRed
mezclaRed _ NoRed _ = NoRed
mezclaRed r1 (R n rs g) cs = mezclaRedAux r1 (traducir rs cs)

mezclaRedAux :: Red -> [Arco] -> Red
mezclaRedAux NoRed _ = NoRed
mezclaRedAux r1 [] = r1
mezclaRedAux r1 (a:as) = mezclaRedAux (mezclaRestr r1 i j r) as
    where
        i = nodo1 a ;
        j = nodo2 a ;
        r = restr a

mezclaRestr :: Red -> Int -> Int -> Restr -> Red
mezclaRestr NoRed  _ _ _ = NoRed
mezclaRestr (R n rs (po,ne)) i j r = actualizaRestr (R m rs g') i j (interseccionBorrosa restRed r)
    where
        m = maximum [n,i,j]
        restRed = devuelveRestr (R n rs (po,ne)) i j
        --NUEVO: Lista de grados de posibilidad y certeza:
        sinInformacion ii jj res1 res2 = ii==jj || (res1==infinito && res2==infinito) 
        entailment x y =  min (necesidad x y) (necesidad y x)
        g' = (posibilidad r restRed:po, if sinInformacion i j restRed r then ne else entailment r restRed :ne)
        
traducir :: [Arco] -> Corresp -> [Arco]
traducir [] cs = []
traducir ((Arc n1 n2 num):rs) cs = (Arc (traduce n1 cs) (traduce n2 cs) num):(traducir rs cs)

traduce :: Int -> Corresp -> Int
traduce n [] = n
traduce n ((x,y):cs)
    | n == x = y
    | otherwise = traduce n cs

-- Comprueba si se está manejando una red o no
-- Nótese que el usuario sólo obtendrá NoRed si mediante alguno de los métodos anteriores
-- se detectó una inconsistencia en las restricciones
esRed :: Red -> Bool
esRed r = r /= NoRed

-- Devuelve el número de nodos
numNodos :: Red -> Int
numNodos NoRed = (-1)
numNodos (R n _ _) = n

-------------------------------------------------------------------
showRed :: Red -> String
showRed NoRed = ""
showRed (R n rs _) = showArcos rs

showArcos ::  [Arco] -> String
showArcos ars = concat ["   "++show v1 ++" "++show v2++" "++ muestra p ++ "\n"| (Arc v1 v2 p)<- ars ]
    where
        muestra p = if p == infinito then "infinite" else show p

showRedes :: [Red] -> String
showRedes []=""
showRedes (rs:rss) = (showRed rs)++"\n------------\n"++showRedes rss

-- Comprueba que una red es igual a otra
esIgual :: Red -> Red -> Bool
esIgual NoRed NoRed = True
esIgual NoRed _ = False
esIgual _ NoRed = False
esIgual (R n1 rs1 g1) (R n2 rs2 g2) = 
    (n1 == n2) && 
        (and [(devuelveRestr (R n1 rs1 g1) i j) == (devuelveRestr (R n2 rs2 g2) i j) | i <- [0..n1], j <- [i..n1]])