module Tarefa1Spec (testesTarefa1) where

import Test.HUnit
import Tarefa1
import LI12324

-- | Colisoes entre personagens

testeA = "TA: Personagens não colidem" ~: False ~=? colisoesPersonagens pl1 pl2
testeB = "TB: Personagens nao colidem" ~: False ~=? colisoesPersonagens pl2 pl4
testeC = "TC: Personagens colidem" ~: True ~=? colisoesPersonagens pl2 pl5


-- | Colisoes com blocos 

testeD = "TD: Personagem colide com plataforma" ~=? colisoesBlocos blocos1 pl3
testeE = "TE: Personagem não colide com blocos" ~=? colicosoesBlocos blocos1 pl4


-- | Colisoes com parede

testeF = "TF: Personagem não colide com limites do mapa" ~=? colisoesParede gameMap1 pl4
testeH = "TH: Personagem colide com limites do mapa" ~=? colisoesParede gameMap1 pl6


pl1 = Personagem (0.0,0.0) Jogador (5.2,1) Oeste (0.8,0.8) False False 10 0 (False, 0.0)
pl2 = Personagem (0.0, 0.0) Fantasma (3,4) Este (1, 1) False True 1 0 (False, 0)
pl3 = Personagem (0.0,0.0) Jogador (0.5,0.5) Este (2,2) False False 10 0 (False, 0.0)
pl4 = Personagem (0.0, 0.0) Jogador (8.5, 6.5) Oeste (0.8, 0.8) False False 10 0 (False, 0)
pl5 = Personagem (0.0, 0.0) Jogador (4,4) Este (1, 1) False True 1 0 (False, 0)
pl6 = Personagem (0.0, 0.0) Fantasma (-3,6) Este (1, 1) False True 1 0 (False, 0)


blocos1 :: [[Bloco]]
blocos1 = [ [ Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
          , [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Alcapao, Plataforma, Plataforma, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]

gameMap1 :: Mapa
gameMap1 = Mapa ((8.5, 6.5), Este) (5, 1.5) blocos1







