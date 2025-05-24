{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Ana Beatriz Ribeiro Freitas <a106853@alunos.uminho.pt>
              José Miguel Fernandes Cação <a106877@alunos.uminho.pt>
Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324
import Tarefa0

{-| =Detecção de colisões

 A função @colisoesPersonagens@ avalia se duas personagens colidem tendo por base o conceito de hitbox.


==Exemplo de Funcionamento:

-}
colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1 p2 = interseta (hitboxPersonagem p1) (hitboxPersonagem p2)


--  A função @colisoesBloco@ avalia se uma personagem colide com algum bloco. Esta testa se a lista das hitboxes dos blocos do tipo Plataformas e Alçapão colide com a hitbox da personagem.
colisoesBlocos :: Mapa -> Personagem -> Bool
colisoesBlocos m@(Mapa _ _ blocos) p = intersetaLista (hitboxListaBlocos (detetaPlataformas (0.5,0.5) blocos)) (hitboxPersonagem p) && 
                                       intersetaLista (hitboxListaBlocos (detetaAlcapoes (0.5,0.5) blocos)) (hitboxPersonagem p)


{- | A função @colisoesParede@ avalia se a personagem colide com algum dos limites do mapa ou com algum bloco de plataforma tendo por base o conceito de hitbox.

==Exemplo de Funcionamento:

-}
colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede m@(Mapa _ _ blocos) p@(Personagem {posicao = (x,y)}) = 
                  x1 <= 0 || y2 <= 0 ||  x2 >= largura || y1 >= altura || colisoesBlocos m p
    where ((x1,y1),(x2,y2)) = hitboxPersonagem p
          ((_,altura),(largura,_)) = hitboxmapa m



