module BorrososTest where

import NumerosBorrosos
import Test.HUnit

x :: NumeroBorroso
x = (1,2,3,4)
y :: NumeroBorroso
y = (5,6,7,8)
inconsistente :: NumeroBorroso
inconsistente = (2,0,4,3)

x1_x2 = (6,8,9,11)
x2_x3 = (-4,-4,-3,-3)
x2_x4 = (-2,0,2,2)
r1 = (5,7,9,11)
r2 = (-8,-4,-1,0)
r3 = (-3,-1,1,3) 

num1 = (5,9,20,30)
num2 = (3,7,20,30)
num3 = (16,16,18,18)
num4 = (18,18,18,18)
num5 = (15,17,19,21)
num6 = (2.5,6.5,10.5,14.5) 
num7 = (2,2,10,10)

ejemplosNumerosBorrosos :: [Test]
ejemplosNumerosBorrosos =
    ["opuestoBorroso (opuestoBorroso x) = x" ~:
        opuestoBorroso (opuestoBorroso x) ~=? x,
     "opuestoBorroso infinito = infinito" ~:
        opuestoBorroso infinito ~=? infinito,
     "sumaBorrosa x y = sumaBorrosa y x" ~:
        sumaBorrosa x y ~=? sumaBorrosa y x,
     "sumaBorrosa infinito x = infinito" ~: 
        sumaBorrosa infinito x ~=? infinito,
     "interseccionBorrosa x y = interseccionBorrosa y x" ~:
        interseccionBorrosa x y ~=? interseccionBorrosa y x,
     "interseccionBorrosa infinito x = x" ~:
        interseccionBorrosa infinito x ~=? x,
     "esConsistente x = true" ~:
        esConsistente x ~=? True,
     "esConsistente inconsistente = false" ~:
        esConsistente inconsistente ~=? False,
     "posibilidad x1 r1 x2 = 1" ~:
        posibilidad x1_x2 r1 ~=? 1,
     "necesidad x1 r1 x2 = 0.5" ~:
        necesidad x1_x2 r1 ~=? 0.5,
     "posibilidad x2 r2 x3 = 1" ~:
        posibilidad x2_x3 r2 ~=? 1,
     "necesidad x2 r2 x3 = 1" ~:
        necesidad x2_x3 r2 ~=? 1,
     "posibilidad x2 r3 x4 = 1" ~:
        posibilidad x2_x4 r3 ~=? 1,
     "necesidad x2 r3 x4 = 0,5" ~:
        necesidad x2_x4 r3 ~=? 0.5,
     "necesidad num1 num2 = 0.5" ~:
        necesidad num1 num2 ~=? 0.5,
     "posibilidad num3 num4 = 1" ~:
        posibilidad num3 num4 ~=? 1,
     "necesidad num3 num4 = 0" ~:
        necesidad num3 num4 ~=? 0,
     "posibilidad num3 num5 = 1" ~:
        posibilidad num3 num5 ~=? 1,
     "necesidad num3 num5 = 0.5" ~:
        necesidad num3 num5 ~=? 0.5,
     "posibilidad num4 num4 = 1" ~:
        posibilidad num4 num4 ~=? 1,
     "necesidad num4 num4 = 1" ~:
        necesidad num4 num4 ~=? 1,
     "necesidad num6 num7 = 0" ~:
        necesidad num6 num7 ~=? 0]

verificaNumerosBorrosos :: IO Counts
verificaNumerosBorrosos = runTestTT (test ejemplosNumerosBorrosos)