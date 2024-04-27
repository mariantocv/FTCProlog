module ManejoTiempo where

import Data.Time
import NumerosBorrosos

-- Representación de una unidad de tiempo
data UnidadTiempo = NoUnidad | Segundo | Minuto | Hora | Dia | Semana | Mes | Año
    deriving (Enum,Eq)

instance Show UnidadTiempo where
    show = showUnidadTiempo

data Signo = Positivo | Negativo
    deriving (Eq)

-- Representación de ua cantidad de tiempo
data CantidadTiempo = CT Signo [(Double,UnidadTiempo)] | NoCantidad
    deriving (Eq)

instance Show CantidadTiempo where
    show = showCantidadTiempo

type CantidadTiempoBorroso = (CantidadTiempo,CantidadTiempo,CantidadTiempo,CantidadTiempo)

type ZonedTimeBorroso = (ZonedTime,ZonedTime,ZonedTime,ZonedTime) 

type UTCTimeBorroso = (UTCTime,UTCTime,UTCTime,UTCTime)

-- Las 0:00 del 1 de enero del año 1
tiempo0 :: ZonedTime
tiempo0 = parseTimeOrError False defaultTimeLocale formato tiempo_str :: ZonedTime
    where
        formato = iso8601DateFormat (Just "%H:%M:%S");
        tiempo_str = "0001-01-01T00:00:00"

-- Las 0:00 del 1 de enero del año 1 en versión borrosa
tiempo0borroso :: ZonedTimeBorroso
tiempo0borroso = (tiempo0,tiempo0,tiempo0,tiempo0)

-- Crea un ZonedTime a partir de un string en formato iso8601
creaTiempo :: String -> ZonedTime
creaTiempo str = parseTimeOrError False defaultTimeLocale formato str :: ZonedTime
    where
        formato = iso8601DateFormat (Just "%H:%M:%S");

-- Crea un ZonedTime a partir de un string, no exlusivamente en formato iso8601
creaTiempoIO :: String -> IO ZonedTime
creaTiempoIO str 
    | str == "now" = getZonedTime
    | str == "tomorrow" = do
                           ahora <- getZonedTime
                           let mañana = sumaTiempo ahora 1 Dia
                           return mañana
    | str == "yesterday" = do 
                          ahora <- getZonedTime
                          let ayer = sumaTiempo ahora (-1) Dia
                          return ayer 
    | otherwise = do return (creaTiempo str)

-- Crea un ZonedTimeBorroso a partir de strings en formato iso8601
creaTiempoBorroso :: (String,String,String,String) -> ZonedTimeBorroso
creaTiempoBorroso (str1,str2,str3,str4) = (creaTiempo str1,
                                           creaTiempo str2,
                                           creaTiempo str3,
                                           creaTiempo str4)

-- Crea un ZonedTimeBorroso a partir de strings, no exclusivamente en formato iso8601
creaTiempoBorrosoIO :: (String,String,String,String) -> IO ZonedTimeBorroso
creaTiempoBorrosoIO (str1,str2,str3,str4) = do
                                              tiempo1 <- creaTiempoIO str1
                                              tiempo2 <- creaTiempoIO str2
                                              tiempo3 <- creaTiempoIO str3
                                              tiempo4 <- creaTiempoIO str4
                                              return (tiempo1,tiempo2,tiempo3,tiempo4)

-- Convierte un ZonedTimeBorroso en su versión en UTC
zonedTimeBorrosoToUTCBorroso :: ZonedTimeBorroso -> UTCTimeBorroso
zonedTimeBorrosoToUTCBorroso (zt1,zt2,zt3,zt4) = (zonedTimeToUTC zt1,
                                                  zonedTimeToUTC zt2,
                                                  zonedTimeToUTC zt3,
                                                  zonedTimeToUTC zt4)

-- Función que dado un número expresado en una unidad de tiempo
-- devuelve una cantidad de tiempo
creaCantidadTiempo :: Double -> UnidadTiempo -> CantidadTiempo
creaCantidadTiempo _ NoUnidad = NoCantidad
creaCantidadTiempo num ut
    | num >= 0 = CT Positivo (creaCTAux ((abs num) * (segundos ut)) Año)
    | otherwise = CT Negativo (creaCTAux ((abs num) * (segundos ut)) Año)

creaCTAux :: Double ->  UnidadTiempo -> [(Double,UnidadTiempo)]
creaCTAux num ut
    | isInfinite num = [(num,ut)]
    | ut == Segundo && num > 0 = [(num,ut)]
    | ut == Segundo && num == 0 = []
    | division >= 1 = (limInf,ut):(creaCTAux (num-(segsUT*limInf)) (anteriorUnidad ut))
    | otherwise = creaCTAux num (anteriorUnidad ut)
        where
            division = num / (realToFrac segsUT);
            segsUT = segundos ut;
            limInf = fromIntegral (floor division);

-- Crea una cantidad de tiempo borrosa a partir de un número boroso y una unidad de tiempo
creaCantidadTiempoBorroso :: NumeroBorroso -> UnidadTiempo -> CantidadTiempoBorroso
creaCantidadTiempoBorroso (a,b,c,d) ut = (creaCantidadTiempo a ut,
                                          creaCantidadTiempo b ut,
                                          creaCantidadTiempo c ut,
                                          creaCantidadTiempo d ut)

-- Obtiene la siguiente unidad de tiempo, de menor a mayor
siguienteUnidad :: UnidadTiempo -> UnidadTiempo
siguienteUnidad NoUnidad = NoUnidad
siguienteUnidad ut = succ ut

-- Obtiene la anterior unidad de tiempo, de menor a mayor
anteriorUnidad :: UnidadTiempo -> UnidadTiempo
anteriorUnidad NoUnidad = NoUnidad
anteriorUnidad ut = pred ut

-- Suma a un tiempo local una cantidad en una determinada unidad de tiempo
sumaTiempo :: ZonedTime -> Double -> UnidadTiempo -> ZonedTime
sumaTiempo zt cant ut = utcToZonedTime timeZone result
    where 
        utcTime = zonedTimeToUTC zt;
        result = addUTCTime (realToFrac (cant * (segundos ut))) utcTime;
        ZonedTime _ timeZone = zt

-- Suma a un tiempo local una cantidad borrosa en una determinada unidad de tiempo
sumaTiempoBorroso :: ZonedTime -> NumeroBorroso -> UnidadTiempo -> ZonedTimeBorroso
sumaTiempoBorroso zt (a,b,c,d) ut = (sumaTiempo zt a ut, 
                                     sumaTiempo zt b ut,
                                     sumaTiempo zt c ut,
                                     sumaTiempo zt d ut)

-- Defuzzifica un ZonedTimeBorroso
defuzzyTime :: ZonedTimeBorroso -> ZonedTime
defuzzyTime (zt1,zt2,zt3,zt4) = utcToZonedTime timeZone media
    where
        utc2 = zonedTimeToUTC zt2;
        utc3 = zonedTimeToUTC zt3;
        ZonedTime _ timeZone = zt2;
        diff = diffUTCTime utc3 utc2;
        media = addUTCTime (diff/2) utc2  

-- Devuelve la cantidad de segundos que corresponde a una unidad de tiempo
segundos :: UnidadTiempo -> Double
segundos NoUnidad = 0
segundos Segundo = 1
segundos Minuto = 60 * (segundos Segundo)
segundos Hora = 60 * (segundos Minuto)
segundos Dia = 24 * (segundos Hora)
segundos Semana = 7 * (segundos Dia)
segundos Mes = 30 * (segundos Dia)
segundos Año = 365 * (segundos Dia)

-- Traduce string a unidad de tiempo
strToUnidad :: String -> UnidadTiempo
strToUnidad str 
    | str == "second" || str == "seconds" = Segundo
    | str == "minute" || str == "minutes" = Minuto
    | str == "hour" || str == "hours" = Hora
    | str == "day" || str == "days" = Dia
    | str == "week" || str == "weeks" = Semana
    | str == "month" || str == "months" = Mes
    | str == "year" || str == "years" = Año
    | otherwise = NoUnidad

-- Indica si es unidad o no 
esUnidad :: UnidadTiempo -> Bool
esUnidad ut = ut /= NoUnidad

----------------------------------------------
showUnidadTiempo :: UnidadTiempo -> String
showUnidadTiempo ut
    | ut == NoUnidad = "unknown"
    | ut == Segundo = "sec"
    | ut == Minuto = "mi"
    | ut == Hora = "h"
    | ut == Dia = "d"
    | ut == Semana = "w"
    | ut == Mes = "mo"
    | ut == Año = "y"


showCantidadTiempo :: CantidadTiempo -> String
showCantidadTiempo NoCantidad = "unknown"
showCantidadTiempo (CT signo []) = '0':(show Segundo)
showCantidadTiempo (CT signo cs) 
    | signo == Positivo = init (concat [ muestra (num,ut) |(num,ut) <- cs])
    | otherwise = init ('-':(concat [ muestra (num,ut) |(num,ut) <- cs]))
    where
        muestra (num,ut)
            | isInfinite num = "infinite "
            | ut == Segundo = (show num) ++ (show ut) ++ " "
            | otherwise = init (init (show num)) ++ (show ut) ++ " "


showTiempo :: ZonedTime -> String
showTiempo zt 
    | utct >= time1 = "inf"
    | utct <= time2 = "-inf"
    | otherwise = show zt
    where
        utct = zonedTimeToUTC zt;
        muchos = 10000000000000000; 
        time1 = zonedTimeToUTC (sumaTiempo tiempo0 muchos Año);
        time2 = zonedTimeToUTC (sumaTiempo tiempo0 (-muchos) Año)

showTiempoBorroso :: ZonedTimeBorroso -> String
showTiempoBorroso (zt1,zt2,zt3,zt4) = "(" ++ showTiempo zt1 ++ ","
                                          ++ showTiempo zt2 ++ ","
                                          ++ showTiempo zt3 ++ ","
                                          ++ showTiempo zt4 ++ ")"