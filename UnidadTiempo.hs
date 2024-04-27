module UnidadTiempo where

import Data.Time

data UnidadTiempo = Segundo | Minuto | Hora | Dia | Semana | Mes | Año | NoUnidad
    deriving (Show,Eq)

-- Suma a un tiempo local una cantidad en una determinada unidad de tiempo
sumaTiempo :: ZonedTime -> Double -> UnidadTiempo -> ZonedTime
sumaTiempo zt cant ut = utcToZonedTime timeZone result
    where 
        utcTime = zonedTimeToUTC zt;
        result = addUTCTime (realToFrac (cant * (segundos ut))) utcTime;
        ZonedTime _ timeZone = zt

-- TODO: Revisar meses y años
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
getUnidades :: String -> UnidadTiempo
getUnidades str 
    | str == "seconds" = Segundo
    | str == "minutes" = Minuto
    | str == "hours" = Hora
    | str == "days" = Dia
    | str == "weeks" = Semana
    | str == "months" = Mes
    | str == "years" = Año
    | otherwise = NoUnidad

-- Indica si es unidad o no 
esUnidad :: UnidadTiempo -> Bool
esUnidad ut = ut /= NoUnidad