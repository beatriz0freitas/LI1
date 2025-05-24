{-|
Module      : Tarefa0
Description : Definição de funções fulcrais para o desenvolvimento do projeto
Copyright   : Ana Beatriz Ribeiro Freitas <a106853@alunos.uminho.pt>
              José Miguel Fernandes Cação <a106877@alunos.uminho.pt>
Módulo para a realização da Tarefa 0 de LI1 em 2023/24.
-}
module Tarefa0 where

import LI12324
import GHC.Float
import Data.List

{- | 

Definição de funções fulcrais para o desenvolvimento do projeto.


pl2 = Personagem (0.0, 0.0) Fantasma (3,4) Este (1, 1) False True 1 0 (False, 0)

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


=HITBOX

A função @hitboxmapa@ define os limites de um mapa tendo por base o conceito de hitbox. 
O retângulo definido que contém o mapa e que interpretamos como sendo a área de colisão entre este e outro qualquer objeto teve como pontos de referência o canto inferior esquerdo e o canto superior direito.

==Exemplo de funcionamento:

>>> hitboxmapa gameMap1
((0.0,12.0),(10.0,0.0))

-}
hitboxmapa :: Mapa -> Hitbox
hitboxmapa (Mapa _ _ blocos) = ((0.0,fromIntegral $ length blocos),(fromIntegral $ length (head blocos), 0.0))


{-| A função @hitboxPersonagem@ delimita uma personagem tendo por base o conceito de hitbox.

== Exemplo de funcionamento:

>>> hitboxPersonagem pl2
((2.5,4.5),(3.5,3.5))

-}
hitboxPersonagem :: Personagem -> Hitbox
hitboxPersonagem personagem = ((x - (l/2),y + (a/2)),(x + (l/2),y - (a/2)))
    where (x,y) = posicao personagem
          (l,a) = tamanho personagem


{- | A função @hitboxDano@ calcula a hitbox Dano de um personagem tendo em conta as suas dimensões e tamanho.

== Exemplo de funcionamento:

>>> hitboxDano pl2
((3.5,4.5),(4.5,3.5))

-}
hitboxDano :: Personagem -> Hitbox
hitboxDano per 
          | dir == Este  = ((x1+l,y1),(x2+l,y2))
          | dir == Oeste = ((x1-l,y1),(x2-l,y2))
          | otherwise    = ((0.0,0.0),(0.0,0.0))
    where dir = direcao per
          ((x1,y1),(x2,y2)) = hitboxPersonagem per
          (l,a) = tamanho per


{-| A função @hitboxUnit@ define os limites de um qualquer bloco unitário da matriz incluindo blocos da matriz, colecionáveis e a estrela.

==Exemplo de funcionamento:

>>> hitboxUnit (3,4)
((2.5,4.5),(3.5,3.5))

-}
hitboxUnit :: Posicao ->  Hitbox
hitboxUnit (x,y) = ((x-0.5,y+0.5),(x+0.5, y-0.5))


-- | A função @hitboxListaBlocos@ calcula a lista de hitboxes de blocos tendo em conta a lista de posiçoes dos mesmos dada.
hitboxListaBlocos :: [Posicao] -> [Hitbox]
hitboxListaBlocos l = map hitboxUnit l


{-| A função @interseta@ deteta colisões entre duas hitboxes, ou seja, avalia se duas hitboxes colidem.

==Exemplos de funcionamento:

>>> interseta ((0.0,0.0),(2.0,2.0)) ((2.0,0.0),(4.0,2.0))
True

>>> interseta ((0.0,0.0),(2.0,2.0)) ((3.0,0.0),(5.0,2.0))
False

-}
interseta :: Hitbox -> Hitbox -> Bool
interseta ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) = not (x2 < x3 || x4 < x1 || y2 > y3 || y4 > y1)


{-| A função @intersetaLista@ averigua se uma dada hitbox colide com qualquer hitbox presente numa lista de hitboxes.

==Exemplos de funcionamento:

>>> intersetaLista [((2.0, 0.0),(4.0,2.0)), ((3.0, 0.0),(5.0, 2.0))] ((0.0,0.0),(2.0,2.0))
True

-}
intersetaLista :: [Hitbox] -> Hitbox -> Bool
intersetaLista [] _ = False
intersetaLista (h:t) p = interseta p h || intersetaLista t p




{-| =MANIPULAÇÃO DA MATRIZ 

A função @mapaTransposto@ calcula o matriz transposta do mapa original.

==Exemplo de funcionamento:

>>>mapaTransposto gameMap1
Mapa ((8.5,6.5),Este) (5.0,1.5) [[Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma],
                                 [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Alcapao,Escada,Escada,Plataforma],
                                 [Plataforma,Vazio,Vazio,Vazio,Vazio,Plataforma,Escada,Escada,Plataforma,Vazio,Vazio,Plataforma],
                                 [Plataforma,Vazio,Plataforma,Escada,Escada,Plataforma,Vazio,Vazio,Plataforma,Vazio,Vazio,Plataforma],
                                 [Plataforma,Vazio,Plataforma,Vazio,Vazio,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma],
                                 [Plataforma,Vazio,Plataforma,Vazio,Vazio,Plataforma,Vazio,Vazio,Plataforma,Vazio,Vazio,Plataforma],
                                 [Plataforma,Vazio,Plataforma,Escada,Escada,Plataforma,Vazio,Vazio,Plataforma,Vazio,Vazio,Plataforma],
                                 [Plataforma,Vazio,Vazio,Vazio,Vazio,Plataforma,Escada,Escada,Plataforma,Vazio,Vazio,Plataforma],
                                 [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Escada,Escada,Plataforma],
                                 [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma]]
-}
mapaTransposto :: Mapa -> Mapa
mapaTransposto (Mapa x y bloco) = Mapa x y (transpose bloco)


{-| A função @encontraBloco@ devolve o tipo de bloco de uma dada posição.

==Exemplo de funcionamento:

>>> encontraBloco (3,4) blocos1
Escada

-}
encontraBloco :: Posicao -> [[Bloco]] -> Bloco
encontraBloco (a,b) m = m !! (double2Int b) !! (double2Int a)


{- | A função @blocoAbaixo@ devolve o tipo de bloco imediatamente abixo de uma posicao dada.

==Exemplo de funcionamento:

>>> blocoAbaixo (9,6) gameMap1
Vazio

-}
blocoAbaixo :: Posicao -> Mapa -> Bloco
blocoAbaixo (x, y) m@(Mapa _ _ blocos) 
    | floor y + 1 < length blocos = encontraBloco (x,y+1) blocos
    | otherwise = Vazio


{-| A função @detetaTipoBlocos@, tendo em conta uma posicao inicial, devolve uma lista com as posicões do bloco n em certa linha da matriz.

==Exemplo de funcionamento:

>>> detetaTipoBlocos (0.5,0.5) Plataforma [Plataforma, Vazio, Escada, Plataforma, Plataforma, Vazio, Plataforma]
[(0.5,0.5),(3.5,0.5),(4.5,0.5),(6.5,0.5)]

-}
detetaTipoBlocos :: Posicao -> Bloco -> [Bloco] -> [Posicao]
detetaTipoBlocos (x,y) n [] = []
detetaTipoBlocos (x,y) n (a:b) | n == a = (x,y) : detetaTipoBlocos (x+1,y) n b
                               | otherwise = detetaTipoBlocos (x+1,y) n b

{-| A função @detetaPlataformas@ devolve uma lista com todas as posicoes dos blocos de tipo Plataforma na matriz.

==Exemplo de funcionamento:

>>> detetaPlataformas (0.5,0.5) blocos1
[(0.5,0.5),(1.5,0.5),(2.5,0.5),(3.5,0.5),(4.5,0.5),(5.5,0.5),(6.5,0.5),(7.5,0.5),(8.5,0.5),(9.5,0.5),(3.5,2.5),(4.5,2.5),(5.5,2.5),(6.5,2.5),(2.5,5.5),(3.5,5.5),(4.5,5.5),(5.5,5.5),(6.5,5.5),(7.5,5.5),(2.5,8.5),(3.5,8.5),(5.5,8.5),(6.5,8.5),(7.5,8.5),(8.5,8.5),(0.5,11.5),(1.5,11.5),(2.5,11.5),(3.5,11.5),(4.5,11.5),(5.5,11.5),(6.5,11.5),(7.5,11.5),(8.5,11.5),(9.5,11.5)]

-}
detetaPlataformas :: Posicao -> [[Bloco]] -> [Posicao]
detetaPlataformas _ [] = []
detetaPlataformas (x,y) (h:t) = detetaTipoBlocos (x,y) Plataforma h ++ detetaPlataformas (x,y+1) t

{-| A função @detetaEscadas@ devolve uma lista com todas as posicoes dos blocos de tipo Escada na matriz.

==Exemplo de funcionamento:

>>> detetaEscadas (0.5,0.5) blocos1
[(3.5,3.5),(6.5,3.5),(3.5,4.5),(6.5,4.5),(2.5,6.5),(7.5,6.5),(2.5,7.5),(7.5,7.5),(1.5,9.5),(8.5,9.5),(1.5,10.5),(8.5,10.5)]

-}
detetaEscadas :: Posicao -> [[Bloco]] -> [Posicao]
detetaEscadas _ [] = []
detetaEscadas (x,y) (h:t) = detetaTipoBlocos (x,y) Escada h ++ detetaEscadas (x,y+1) t

{-| A função @detetaAlcapao@ devolve uma lista com todas as posicoes dos blocos de tipo Alcapao na matriz.

==Exemplo de funcionamento:

>>> detetaAlcapoes (0.5,0.5) blocos1
[(1.5,8.5)]

-}
detetaAlcapoes :: Posicao -> [[Bloco]] -> [Posicao]
detetaAlcapoes _ [] = []
detetaAlcapoes (x,y) (h:t) = detetaTipoBlocos (x,y) Alcapao h ++ detetaAlcapoes (x,y+1) t


{-| A função @agruparBlocosSeguidos@ agrupa posicoes de blocos tendo em conta a sua abcissa no bloco. --group
-}
agruparBlocosSeguidos :: [Posicao] -> [[Posicao]]
agruparBlocosSeguidos [] = []
agruparBlocosSeguidos [x] = [[x]]
agruparBlocosSeguidos ((x,y):t)
                        | (x+1,y) `elem` head r = ((x,y) : head r) : tail r
                        | otherwise = [(x,y)] : r
                  where r = agruparBlocosSeguidos t


-- | A função  @agruparAlcapoes@ agrupa os alçapões do mapa tendo em conta as suas posições no mapa.
agruparAlcapoes :: Mapa -> [[Posicao]]
agruparAlcapoes m@(Mapa _ _ blocos) = agruparBlocosSeguidos (detetaAlcapoes (0.5, 0.5) blocos)


{-| A função @agruparEscadasPorY@ 

-}
agruparEscadasPorY :: Mapa -> [[Posicao]]
agruparEscadasPorY m@(Mapa _ _ blocos) = filter (not . null) $ agruparEscadasPorY' blocos 0 


-- | A função @agrupaEscadasPorY'@ analisa todo mapa tendo em conta a função @encontrarEscadas@.
agruparEscadasPorY' :: [[Bloco]] -> Double -> [[Posicao]]
agruparEscadasPorY' blocos y = map (encontrarCentroEscadas y) blocos


-- | A função @encontrarCentroEscadas@ obtem os índices dos elementos Escada em cada linha do mapa e, depois de mapear esses índices para as posições desejadas na linha, adiciona 0.5 a cada índice para representar a posição central da escada.
encontrarCentroEscadas :: Double -> [Bloco] -> [Posicao]
encontrarCentroEscadas x linha =  map (\y -> (fromIntegral y + 0.5, x)) $ elemIndices Escada linha




-- | A função @blocoAbaixoGloss@ 
blocoAbaixoGloss :: Posicao -> Mapa -> Bloco
blocoAbaixoGloss (x,y) m@(Mapa _ _ bloco) = encontraBloco (x,y+0.5) bloco