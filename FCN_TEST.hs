module FCN_TEST where

import NumerosBorrosos
import FCN
import FTCNE3
import Test.HUnit

ejemplosRedes :: [Test]
ejemplosRedes =
    ["Igualdad de redes escritas con restricciones redundantes" ~:
        red ~=? redRedundante, 
    "Devolver una restricción universal" ~: 
        devuelveRestr red 0 0 ~=? infinito,
     "Devolver una restricción explícita" ~:
        devuelveRestr red 0 1 ~=? (12,14,16,18),
     "Devolver una restricción inversa no explícita" ~:
        devuelveRestr red 2 1 ~=? (-7,-6,-4,-3),
     "Red mínima de la red" ~:
        minimizaRed red ~=? minima,
     "La minima de la redundante debe ser igual a la minima" ~:
        minimizaRed redRedundante ~=? minima,
     "La mínima de la mínima debe seguir siendo la mínima" ~:
        minimizaRed minima ~=? minima,
     "Comprobacion de resultados de ejemplo teorico (1/2)" ~:
        mezclaRed patron inicio correspInicioPatron ~=? mezclaInicioPatron,
     "Comprobacion de resultados de ejemplo teorico (2/2)" ~:
        minimizaRed (mezclaRed mezclaInicioPatron minima []) ~=? result,
     "Minimizacion encuentra inconsistencia en la red (1/2)" ~:
        esRed (minimizaRed inconsistente1) ~=? False,
     "Minimizacion encuentra inconsistencia en la red (2/2)" ~:
        esRed (minimizaRed inconsistente2) ~=? False]

verificaRedes :: IO Counts
verificaRedes = runTestTT (test ejemplosRedes)