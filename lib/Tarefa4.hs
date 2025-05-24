{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Ana Beatriz Ribeiro Freitas <a106853@alunos.uminho.pt>
              José Miguel Fernandes Cação <a106877@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe

import LI12324
import Tarefa0
import Tarefa1
import Tarefa2
import Tarefa3




atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza acoesInimigos acaoPersonagem jogo =
  jogo {{-inimigos = atualizaInimigos acoesInimigos jogo,-} jogador = atualizaJogador acaoPersonagem jogo}

velocidadeMovimento :: Double
velocidadeMovimento = 3

atualizaJogador :: Maybe Acao -> Jogo -> Personagem
atualizaJogador acao jogo = case acao of
  Just AndarDireita -> andarParaDireita jogo
  Just AndarEsquerda -> andarParaEsquerda jogo
  Just Subir -> andarParaCima jogo
  Just Descer -> andarParaBaixo jogo
  Just Saltar -> salto jogo
  Just Parar -> parar jogo
  _ -> p
  where
    p = jogador jogo



andarParaDireita :: Jogo -> Personagem
andarParaDireita j
  | blocoAbaixo pos m == Plataforma || blocoAbaixo pos m == Escada = p {velocidade = (velocidadeMovimento, vy), direcao = Este }
  | otherwise = p
  where
    p = jogador j
    pos = posicao p
    m@(Mapa _ _ bloco) = mapa j
    (vx, vy) = velocidade p



andarParaEsquerda :: Jogo -> Personagem
andarParaEsquerda j 
  | blocoAbaixo pos m /= Vazio  = p {velocidade = (-velocidadeMovimento, vy), direcao = Oeste}
  | otherwise = p
  where
    p = jogador j
    pos = posicao p
    m@(Mapa _ _ bloco) = mapa j
    (vx, vy) = velocidade p


andarParaCima :: Jogo -> Personagem
andarParaCima j | emEscada p = p { velocidade = (vx, -4), direcao = Norte}
                | otherwise = p
  {-| emEscada p == True = p { velocidade = (vx, -velocidadeMovimento), direcao = Norte, emEscada = True }
  | otherwise = p-}
  where
    p = jogador j
    pos = posicao p
    m@(Mapa _ _ bloco) = mapa j
    (vx, vy) = velocidade p



andarParaBaixo :: Jogo -> Personagem
andarParaBaixo j 
  | emEscada p == True && not (blocoAbaixoGloss pos m == Plataforma) = p { velocidade = (vx, 4), direcao = Sul }
  | emEscada p == True && blocoAbaixoGloss pos m == Plataforma =p { velocidade = (vx, vy), direcao = Sul }
  | otherwise = p
  where
    p = jogador j
    pos = posicao p
    m@(Mapa _ _ bloco) = mapa j
    (vx, vy) = velocidade p



salto :: Jogo -> Personagem
salto j | emEscada p == False && (blocoAbaixo pos (mapa j) == Plataforma || blocoAbaixo pos (mapa j) == Alcapao) = p {velocidade = (vx, -4)} 
        | otherwise = p
  where
    p = jogador j
    pos = posicao p
    m@(Mapa _ _ bloco) = mapa j
    (vx, vy) = velocidade p


--- definir colisoesJogador em funcao de parar
parar :: Jogo -> Personagem
parar j | not $ emEscada p =  p {velocidade = (0,0)}
        | otherwise = p
   where 
    p = jogador j
    pos = posicao p
    m@(Mapa _ _ bloco) = mapa j
    (vx, vy) = velocidade p






---- rever para nao repetir informacao tarefa3
{-
atualizaInimigo :: Maybe Acao -> Jogo -> Personagem
atualizaInimigo acao jogo = case acao of
  Just AndarDireita -> andarParaDireitainimigos jogo
  Just AndarEsquerda -> andarParaEsquerdaInimigos jogo
  Just Subir -> andarParaCimaInimigos jogo
  Just Descer -> andarParaBaixoInimigos jogo
  Just Saltar -> saltoInimigos jogo
  Just Parar -> pararInimigos jogo
  _ -> p
  where
    p = head (inimigos jogo)



atualizaInimigos :: [Maybe Acao] -> Jogo -> [Personagem]
atualizaInimigos [] _ = []
atualizaInimigos (h : t) jogo = atualizaInimigo h jogo : atualizaInimigos t jogo



andarParaDireitainimigos :: Jogo -> Personagem
andarParaDireitainimigos j
  | not (emEscada p) && blocoAbaixo pos m == Plataforma = p {velocidade = (velocidadeMovimento, vy), direcao = Este}
  | otherwise = p
  where
    p = head (inimigos j)
    pos = posicao p
    m@(Mapa _ _ bloco) = mapa j
    (vx, vy) = velocidade p



andarParaEsquerdaInimigos :: Jogo -> Personagem
andarParaEsquerdaInimigos j
  | not (emEscada p) && blocoAbaixo pos m == Plataforma = p {velocidade = (-velocidadeMovimento, vy), direcao = Oeste}
  | otherwise = p
  where
    p = head (inimigos j)
    pos = posicao p
    m@(Mapa _ _ bloco) = mapa j
    (vx, vy) = velocidade p



andarParaCimaInimigos :: Jogo -> Personagem
andarParaCimaInimigos j
  | emEscada p = p {velocidade = (vx, -velocidadeMovimento), direcao = Norte}
  | otherwise = p
  where
    p = head (inimigos j)
    pos = posicao p
    m@(Mapa _ _ bloco) = mapa j
    (vx, vy) = velocidade p



andarParaBaixoInimigos :: Jogo -> Personagem
andarParaBaixoInimigos j
  | emEscada p = p {velocidade = (vx, velocidadeMovimento), direcao = Sul}
  | otherwise = p
  where
    p = head (inimigos j)
    pos = posicao p
    m@(Mapa _ _ bloco) = mapa j
    (vx, vy) = velocidade p


{-
saltoInimigos :: Jogo -> Personagem
saltoInimigos j
  | not (emEscada p) && (blocoAbaixo pos m == Plataforma || blocoAbaixo pos m == Alcapao) = p {velocidade = (vx, -velocidadeMovimento)}
  | otherwise = p
  where
    p = head (inimigos j)
    pos = posicao p
    m@(Mapa _ _ bloco) = mapa j
    (vx, vy) = velocidade p



pararInimigos :: Jogo -> Personagem
pararInimigos j
  | emEscada p = p {velocidade = (0, 0)}
  | otherwise = p
  where
    p = head (inimigos j)
    pos = posicao p
    m@(Mapa _ _ bloco) = mapa j
    (vx, vy) = velocidade p
-}-}
