module RelacionesTest where

import Relaciones
import EjemplosRelaciones
import NumerosBorrosos

import Test.HUnit


ejemplosRelaciones :: [Test]
ejemplosRelaciones =
    ["posibilidad x1 r1 x2 = 1" ~:
        posibilidad x1_x2 (getDist r1) ~=? 1,
     "necesidad x1 r1 x2 = 0.5" ~:
        necesidad x1_x2 (getDist r1) ~=? 0.5,
     "posibilidad x2 r2 x3 = 1" ~:
        posibilidad x2_x3 (getDist r2) ~=? 1,
     "necesidad x2 r2 x3 = 1" ~:
        necesidad x2_x3 (getDist r2) ~=? 1,
     "posibilidad x2 r3 x4 = 1" ~:
        posibilidad x2_x4 (getDist r3) ~=? 1,
     "necesidad x2 r3 x4 = 0,5" ~:
        necesidad x2_x4 (getDist r3) ~=? 0.5]

verificaRelaciones :: IO Counts
verificaRelaciones = runTestTT (test ejemplosRelaciones)