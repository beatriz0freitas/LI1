module Main where

import Tarefa1Spec
import Tarefa2Spec
import Tarefa3Spec
import Tarefa4Spec
import Test.HUnit
import Fixtures.Mapas
import Fixtures.Personagens

main :: IO ()
main = 
    runTestTTAndExit $ 
        test [testesTarefa1, 
              testesTarefa2, 
              testesTarefa3, 
              testesTarefa4
              ]


test_suite_01 = test ["Basic Test" ~: True ~=? True]

