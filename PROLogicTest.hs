module PROLogicTest where

import PROLogic 
import EjemplosPROLogic
import Test.HUnit

ejemplosPROLogic :: [Test]
ejemplosPROLogic =
    ["Problema teórico del diagnóstico" ~:
        respuestasReducidasAmp programa objetivo ~=? solucion,
     "Problema teórico del asesinato en el museo" ~:
        respuestasReducidasAmp programa2 objetivoPolicia ~=? solucion2,
     "Programa con igualdad 1" ~:
     	respuestasReducidasAmp programaIgual objetivoIgual1 ~=? solucionIgual1,
     "Programa con igualdad 2" ~:
        respuestasReducidasAmp programaIgual objetivoIgual2 ~=? solucionIgual2]

verificaPROLogic :: IO Counts
verificaPROLogic = runTestTT (test ejemplosPROLogic)