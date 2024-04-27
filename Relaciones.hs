module Relaciones where

import ManejoTiempo
import NumerosBorrosos

data Relacion = R NumeroBorroso UnidadTiempo
type Modificador = Relacion -> Relacion

instance Show Relacion where
    show = showRel

-- El infinito
inf :: Double
inf = 1/0

----------------------CUANTITATIVAS-----------------------

-- Modificador MAS DE
mas_de :: Modificador
mas_de (R nb ut) = R (sumaBorrosa nb (1,1,inf,inf)) ut

-- Modificador MENOS DE
menos_de :: Modificador
menos_de (R nb ut) = R (sumaBorrosa nb (-inf,-inf,-1,-1)) ut

-- Modificador APROXIMADAMENTE
aproximadamente :: Modificador
aproximadamente (R nb ut) = R (sumaBorrosa nb (-3,-1,1,3)) ut

-- Relación para una cantidad de tiempo
cantidad :: Double -> UnidadTiempo -> Relacion
cantidad num ut = cantidadBorrosa (num,num,num,num) ut

-- Relación para una cantidad borrosa de tiempo
cantidadBorrosa :: NumeroBorroso -> UnidadTiempo -> Relacion
cantidadBorrosa num ut = R num ut

----------------------CUALITATIVAS-----------------------

-- Relacion ANTES
antes :: UnidadTiempo -> Relacion
antes ut = R (0,0.1,inf,inf) ut

-- Modificador ANTES
antesM :: Modificador
antesM (R nb ut) = R nb ut

-- Relacion DESPUES
despues :: UnidadTiempo -> Relacion 
despues ut = R (-inf,-inf,-0.1,0) ut

-- Modificador DESPUES
despuesM :: Modificador
despuesM (R nb ut) = R (opuestoBorroso nb) ut

-- Relacion AL MISMO TIEMPO
al_mismo_tiempo :: UnidadTiempo -> Relacion
al_mismo_tiempo ut = R (0,0,0,0) ut

-- Relacion MUCHO
mucho :: UnidadTiempo -> Relacion
mucho ut = R (0,10,inf,inf) ut

-- Relacion POCO
poco :: UnidadTiempo -> Relacion
poco ut = R (0,1,4,8) ut

----------------------CALCULO-----------------------

-- Mezcla 2 relaciones intersecandolas
mezclaRelacion :: Relacion -> Relacion -> Relacion
mezclaRelacion (R nb1 ut1) (R nb2 ut2) = 
    if (ut1 == ut2) then 
        R (interseccionBorrosa nb1 nb2) ut1
    else
        R (interseccionBorrosa nbs1 nbs2) segs
    where
        segs = strToUnidad "seconds";
        nbs1 = escala nb1 (segundos ut1);
        nbs2 = escala nb2 (segundos ut2)

-- Une 2 relaciones mediante la operación de unión
uneRelacion :: Relacion -> Relacion -> Relacion
uneRelacion (R nb1 ut1) (R nb2 ut2) =
    if (ut1 == ut2) then
        R (unionBorrosa nb1 nb2) ut1
    else
        R (unionBorrosa nbs1 nbs2) segs
    where
        segs = strToUnidad "seconds";
        nbs1 = escala nb1 (segundos ut1);
        nbs2 = escala nb2 (segundos ut2) 

-- Obtiene la distribución de una relacion
getDist :: Relacion -> NumeroBorroso
getDist (R nb _) = nb

-- Obtiene la unidad de tiempo de una relacion
getUT :: Relacion -> UnidadTiempo
getUT (R _ ut) = ut

showRel :: Relacion -> String
showRel (R nb ut) = (show nb) ++ " " ++ (show ut)