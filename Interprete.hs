module Interprete where

import PROLogic
import CommandParser
import ImpComandos
import Exception
import Control.Exception
import System.IO.Error

-- Inicia el intérprete en el contexto de un programa sin cláusulas
interprete :: IO()
interprete = do putStrLn "FTCProlog 2.0, 2024 (previous version PROLogic 1.0, 2017). Use \"help help.\" for help"
                interpreteEs estadoInicial

-- Ejecución del interprete dado el estado del entorno de ejecucion
interpreteEs :: Estado -> IO()
interpreteEs es 
    | esFinal es = 
        do
            putStrLn ""
    | otherwise =
        handle (handler es) $ do 
            putStr "?-"
            entrada <- getLine
            interpreteEsAux es entrada

interpreteEsAux :: Estado -> String -> IO()
interpreteEsAux es entrada 
    | null entrada = interpreteEs es 
    | (last entrada) /= '.' = 
        do
            putStr "?-"
            siguiente <- getLine  
            interpreteEsAux es (entrada++siguiente)
    | otherwise =
        do
            let consulta = parse entrada
            procesaConsulta es consulta >>= interpreteEs

procesaConsulta :: Estado -> Ex Consulta -> IO Estado
procesaConsulta es (Ok consulta) = ejecuta es consulta
procesaConsulta es (Failed err) = 
    do 
        putStrLn err
        return es

handler :: Estado -> SomeException -> IO()
handler es ex = 
    do
        putStrLn (show ex) 
        interpreteEs es