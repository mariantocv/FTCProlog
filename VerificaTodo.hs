module VerificaTodo where

import BorrososTest
import FCN_TEST
import ManejadorTest
import PROLogicTest
import RelacionesTest

import Test.HUnit

verificaTodo =
    runTestTT (test ((map (TestLabel "NumerosBorrosos")
                       ejemplosNumerosBorrosos) ++
                     (map (TestLabel "Redes")
                       ejemplosRedes) ++
                     (map (TestLabel "ManejadorRedes")
                       ejemplosManejadorRedes) ++
                     (map (TestLabel "PROLogic")
                       ejemplosPROLogic) ++
                     (map (TestLabel "Relaciones")
                       ejemplosRelaciones)))