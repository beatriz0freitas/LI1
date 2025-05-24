{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Ana Beatriz Ribeiro Freitas <a106853@alunos.uminho.pt>
              José Miguel Fernandes Cação <a106877@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324
import Tarefa0
import Tarefa1
import Data.List
import GHC.Float

{-| =Validação do mapa

A função valida garante que nenhum jogo viola as restrições definidas.
-}
pl1 = Personagem (0.0,0.0) Jogador (5.2,1) Oeste (0.8,0.8) False False 10 0 (False, 0.0)
pl2 = Personagem (0.0, 0.0) Fantasma (3,4) Este (1, 1) False True 1 0 (False, 0)
pl3 = Personagem (0.0,0.0) Jogador (0.5,0.5) Este (2,2) False False 10 0 (False, 0.0)
pl4 = Personagem (0.0, 0.0) Jogador (8.5, 6.5) Oeste (0.8, 0.8) False False 10 0 (False, 0)
pl5 = Personagem (0.0, 0.0) Jogador (4,4) Este (1, 1) False True 1 0 (False, 0)
pl6 = Personagem (0.0, 0.0) Fantasma (-3,6) Este (1, 1) False True 1 0 (False, 0)

col1 = (Martelo, (3,4))
col2 = (Moeda, (5,4))
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

jogo01 :: Jogo
jogo01 =
  Jogo
    { mapa = gameMap1,
      inimigos = [pl2, pl6],
      colecionaveis = [col1, col2],
      jogador = pl3
    }


valida :: Jogo -> Bool
valida (Jogo mapa inimigos colecionaveis jogador _) =
  chao mapa &&
  verificaRessaltaInimigos inimigos &&
  verificaRessaltaJogador jogador &&
  posicaoInicial inimigos jogador &&
  numeroInimigos inimigos &&
  inimigosFantasma inimigos &&
  validaEscadasMapa mapa  &&
  validaAlcapoes mapa jogador &&
  verificaPosicao mapa colecionaveis jogador


{- | A função @chao@ verifica se existe uma plataforma que impede que o jogador ou outro personagem caia fora do mapa.

==Exemplo de funcionamento:

>>> chao gameMap1
True

-}
chao :: Mapa -> Bool
chao (Mapa _ _ blocos) = all (== Plataforma) (last blocos)


{- | A função @ressaltac@ verifica, se a personagem for:
        *INIMIGO, que a sua propriedade ressalta está a True
        *JOGADOR, que a sua propriedade ressalta está a False
-}
ressaltac :: Personagem -> Bool
ressaltac per = (tipo per == Jogador && not (ressalta per)) || (tipo per == Fantasma && ressalta per)

{- | A função @verificaRessaltaInimigos@ averigua se todos os inimigos têm a propriedade 'ressalta' a True.

==Exemplo de funcionamento:

>>> verificaRessaltaInimigos (inimigos jogo01)
True

-}
verificaRessaltaInimigos :: [Personagem] -> Bool
verificaRessaltaInimigos l = all ressaltac l


{- | A função @verificaRessaltaJogador@ garante que o jogador tem a propriedade 'ressalta' a False.

==Exemplo de funcionamento:

>>> verificaRessaltaJogador (jogador jogo01)
True

-}
verificaRessaltaJogador :: Personagem -> Bool
verificaRessaltaJogador l = ressaltac l


{- | A função @posicaoInicial@ testa se a posicao inicial do jogador colide com a posicão de outro personagem.

==Exemplo de funcionamento:

>>> posicaoInicial (inimigos jogo01) (jogador jogo01)
True

-}
posicaoInicial :: [Personagem] -> Personagem -> Bool
posicaoInicial inimigos jogador = all (\inimigo -> not $ colisoesPersonagens inimigo jogador) inimigos


{- | A função @numeroInimigos@ verifica se o número mínimo de inimigos é 2.

== Exemplo de funcionamento:

>>> numeroInimigos (inimigos jogo01)
True

-}
numeroInimigos :: [Personagem] -> Bool
numeroInimigos n = length n >= 2


{- | A função @inimigosFantasma@ verifica se os inimigos com entidade Fantasma têm exatamente 1 vida.

== Exemplo de funcionamento:

>>> inimigosFantasma (inimigos jogo01)
True

-}
inimigosFantasma :: [Personagem] -> Bool
inimigosFantasma per = all (\f -> vida f == 1) fantasmas
        where fantasmas = filter (\v -> tipo v == Fantasma) per



validaEscadasMapa :: Mapa -> Bool
validaEscadasMapa m@(Mapa _ _ bloco) = validaEscadas1 k m && validaEscadas2 k m
        where k = agruparEscadasPorY (mapaTransposto m)  

validaEscadas1 :: [[Posicao]] -> Mapa -> Bool
validaEscadas1 [] _ = True
validaEscadas1 (h:t) m@(Mapa _ _ bloco) = (notElem (x, y - 1) (detetaAlcapoes (0.5, 0.5) bloco) && 
                                           notElem (x',y'+1) (detetaAlcapoes (0.5, 0.5) bloco)) && 
                                          validaEscadas1 t m
        where (y,x) = head h 
              (y',x') = last h

validaEscadas2 :: [[Posicao]] -> Mapa -> Bool
validaEscadas2 [] _ = True
validaEscadas2 (h:t) m@(Mapa _ _ bloco) = ((x,y-1) `elem` detetaPlataformas (0.5,0.5) bloco || 
                                           (x',y'+1) `elem` detetaPlataformas (0.5,0.5) bloco) && 
                                          validaEscadas2 t m
        where (y,x) = head h 
              (y',x') = last h



{- | A função @validaAlcapoes@ garante que o jogador passa pelo alçapão, ou seja, que o alcapão não é menos largo que o jogador.

==Exemplo de funcionamento:

>>>validaAlcapoes gameMap1 (jogador jogo01)
False

>>> validaAlcapoes gameMap1 pl1
True

-}
validaAlcapoes :: Mapa -> Personagem -> Bool
validaAlcapoes m@(Mapa _ _ blocos) p = all (\l -> fromIntegral (length l) > largura) alcapoesMatriz
                                where alcapoesMatriz = agruparAlcapoes m
                                      (largura, altura) = tamanho p


{- | A função @verificaPosicaoColecionaveis@ garante que todos os colecionáveis do jogo estão em blocos vazios do mapa.

==Exemplo de funcionamento:

>>> verificaPosicaoColecionaveis (colecionaveis jogo01) blocos1
False

-}
verificaPosicaoColecionaveis :: [(Colecionavel,Posicao)] -> [[Bloco]] -> Bool
verificaPosicaoColecionaveis colecionaveis m = all (==Vazio) (map (\(_,pos) -> encontraBloco pos m) colecionaveis) 


{- | A função @verificaPosicaoJogador@ garante que a posição do jogador corresponde a um bloco Vazio.

==Exemplo de funcionamento:

>>> verificaPosicaoJogador (jogador jogo01) blocos1
False

>>> verificaPosicaoJogador pl1 blocos1
True

-}
verificaPosicaoJogador :: Personagem -> [[Bloco]] -> Bool
verificaPosicaoJogador p m = encontraBloco x m == Vazio
        where x = posicao p


{- | A função @verificaPosicao@ averigua se o bloco correspendente à posição de qualquer personagem ou objeto é Vazio.

==Exemplo de funcionamento:

>>> verificaPosicao gameMap1 (colecionaveis jogo01) (jogador jogo01)
False

-}
verificaPosicao :: Mapa -> [(Colecionavel,Posicao)] -> Personagem -> Bool
verificaPosicao mapa@(Mapa _ _ bloco) colecionaveis personagem = verificaPosicaoJogador personagem bloco && 
                                                                 verificaPosicaoColecionaveis colecionaveis bloco



