{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Ana Beatriz Ribeiro Freitas <a106853@alunos.uminho.pt>
              José Miguel Fernandes Cação <a106877@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324
import Tarefa0
import Tarefa1
import Data.Ord
import Data.List
import System.Random


movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta semente tempo jogo@(Jogo mapa inimigos colecionaveis jogador pausa) 
      | pausa = jogo
      | otherwise = inimigoColideDanoJogo $ 
                    morteInimigosJogo $ 
                    gravidadeJogo tempo $ 
                    jogadorAtingidoJogo $ 
                    recolheColecionavelJogo $ 
                    countdownTempoMarteloJogo tempo $
                    jogadorPisaAlcapaoJogo $ 
                    limitacoesInimigosJogo tempo $ 
                    movimentaJogadorJogo tempo $
                    centraJogadorEmEscadaJogo $
                    movimentaJogadorJogo tempo $
                    alteraPosicoesColecionaveisJogo $
                    atualizaEmEscadaJogadorJogo $
                    emEscadaPosicao jogo 



{--}
emEscadaPosicao :: Jogo -> Jogo
emEscadaPosicao jogo = jogo {jogador = (jogador jogo) {velocidade = emEscadaPosiccaoaux (velocidade (jogador jogo)) (emEscada (jogador jogo))}}
--- ?
emEscadaPosicaoaux :: Velocidade -> Bool -> Velocidade
emEscadaPosicaoaux velocidade bool     | bool = (if snd velocidade == 4 || snd velocidade == -4 then 0 else fst velocidade,snd(velocidade))
                                        | otherwise = velocidade

-------------- 1 --------------

-- | A função @jogadorArmado@ define as condições de um jogador Armado.
jogadorArmado :: Personagem -> Bool
jogadorArmado Personagem{aplicaDano = (dano, tRestante)} = dano && tRestante > 0


-- | A função @inimigoColideDano@ verifica, quando o jogador está armado, se inimigos colidem com a sua hitbox Dano. Caso aconteça os inimigos perdem uma vida.
inimigoColideDano :: [Personagem] -> Personagem -> [Personagem]
inimigoColideDano [] jogador = []
inimigoColideDano l@(inimigo:t) jogador
              | jogadorArmado jogador && interseta (hitboxPersonagem inimigo) (hitboxDano jogador) = inimigo {vida = max 0 (vida inimigo - 1)} : inimigoColideDano t jogador
              | otherwise                                                                          = inimigoColideDano t jogador


inimigoColideDanoJogo :: Jogo -> Jogo
inimigoColideDanoJogo jogo = 
                  Jogo {
                        mapa = mapa jogo, 
                        inimigos = inimigoColideDano (inimigos jogo) (jogador jogo), 
                        colecionaveis = colecionaveis jogo, 
                        jogador = jogador jogo,
                        pausa = False
                        jogador = jogador jogo,
                        pausa = False
                        }



-------------- 2 -------------

morte :: Personagem -> Bool
morte inimigo = vida inimigo == 0


desaparece :: Personagem -> Mapa -> Posicao
desaparece j m@(Mapa _ _ bloco) | morte j = (x+a, y+b)
                                | otherwise = (x,y)
               where (x,y) = posicao j
                     ((_,b),(a,_)) = hitboxmapa m


-- | A função @inimigoMorto@ transporta um inimigo morto para fora do mapa.
inimigoMorto :: Mapa -> Personagem -> Personagem
inimigoMorto m inimigo = inimigo {posicao = desaparece inimigo m, vida = 0}
                  where (x,y) = posicao inimigo


morteInimigos :: Mapa -> [Personagem] -> [Personagem]
morteInimigos m l = map (inimigoMorto m) l 


morteInimigosJogo :: Jogo -> Jogo
morteInimigosJogo jogo = 
             Jogo { mapa = mapa jogo,
                    inimigos = morteInimigos (mapa jogo) (inimigos jogo), 
                    colecionaveis = colecionaveis jogo, 
                    jogador = jogador jogo,
                    pausa = False
                  }



------------ 3 ----------------

-- | A função @mudarPosicao@
mudarPosicao :: Tempo -> Personagem -> Personagem
mudarPosicao t p = p {velocidade = (fst (velocidade p), snd (velocidade p) + t*(snd gravidade))}
mudarPosicao t p = p {velocidade = (fst (velocidade p), snd (velocidade p) + t*(snd gravidade))}


-- | A função @gravidadezinha@ qualquer personagem que não esteja sobre uma plataforma ou que nao esteja numa escada deverá “cair”.
gravidadezinha :: Mapa -> Tempo -> Personagem -> Personagem
gravidadezinha m@(Mapa _ _ bloco) t p
              | blocoAbaixoGloss (posicao p) m /= Plataforma && not (emEscada p) = mudarPosicao t p
              | otherwise                                                        = p{velocidade = (a,if emEscada p then b else 0),
                                                                                     posicao = (x,if direcao p == Norte then y-t*b 
                                                                                                  else if direcao p == Sul then y+t*b 
                                                                                                  else y)
                                                                                     }
              where (a,b) = velocidade p
                    (x,y) = posicao p
               
gravidadeJogo :: Tempo -> Jogo -> Jogo
gravidadeJogo t jogo = 
             Jogo {
                  mapa = mapa jogo, 
                  inimigos = inimigos jogo, 
                  colecionaveis = colecionaveis jogo, 
                  jogador = gravidadezinha (mapa jogo) t (jogador jogo),
                  pausa = False
                  }
{-}
seDentroSai :: Personagem -> Mapa -> Personagem
seDentroSai jogadro mapa | 
                         | otherwise = jogaador
{-}
seDentroSai :: Personagem -> Mapa -> Personagem
seDentroSai jogadro mapa | 
                         | otherwise = jogaador



-}

-}

------------ 4 --------------

-- | A função @jogadorAtingido@ remove uma vida ao jogador sempre que este é atingido por qualquer inimigo e transporta-o para a posição inicial.
jogadorAtingido :: Mapa -> [Personagem] -> Personagem -> Personagem
jogadorAtingido _ [] jogador = jogador
jogadorAtingido m@(Mapa (pi,_) _ _) l@(inimigo:t) jogador
              | interseta (hitboxPersonagem inimigo) (hitboxDano jogador) = jogador {vida = max 0 (vida jogador - 1), posicao = pi}
              | otherwise                                                 = jogadorAtingido m t jogador

-- | A função @jogadorArmadoJogo@ fornece ao Jogo a reação do jogador ao ataque do inimigo.
jogadorAtingidoJogo :: Jogo -> Jogo
jogadorAtingidoJogo jogo = 
                   Jogo {
                        mapa = mapa jogo, 
                        inimigos = inimigos jogo, 
                        colecionaveis = colecionaveis jogo,
                        jogador = jogadorAtingido (mapa jogo) (inimigos jogo) (jogador jogo),
                        pausa = False
                        }


------------ 5 -------------

{-
ACHO QUE ESTA FUNÇÃO SERÁ INUTIL PORQUE OS COLECIONAVEIS GASTOS PODEM SER ELIMINADOS DA LISTA
-- | A função @colecionavelGasto@ retira o colecionavel gasto do mapa.
colecionavelGasto :: Mapa -> Personagem -> (Colecionavel, Posicao) -> (Colecionavel, Posicao)
colecionavelGasto m jogador c@(colecionavel,(x,y))
              | interseta (hitboxPersonagem jogador) (hitboxUnit (x,y)) = (colecionavel,desaparece m (x,y))
              | otherwise                                               = c
-}


-- | A função @colecionaveisDisponiveis@ gera uma lista dos colecionaveis ainda disponiveis para o jogador no mapa.
colecionaveisDisponiveis :: Personagem -> [(Colecionavel, Posicao)] -> [(Colecionavel, Posicao)]
colecionaveisDisponiveis jogador colecionaveis =  filter (\(c, pCol) -> interseta (hitboxPersonagem jogador) (hitboxUnit pCol)) colecionaveis
---- CONFIRMAR SE RODA


-- | A função @efeitoColecionavel@ aplica ao jogador os efeitos do colecionavel.
efeitoColecionavel :: Personagem -> (Colecionavel, Posicao) -> Personagem
efeitoColecionavel jogador (colecionavel, _)
              | colecionavel == Martelo = jogador { aplicaDano = (True, 10)}
              | colecionavel == Moeda   = jogador { pontos = pontos jogador + 1 }


-- | A função @aplicarColecionavel@ devolve o jogador depois de lhe ser aplicado todos os colecionáveis disponiveis.
aplicarColecionavel :: Personagem -> [(Colecionavel, Posicao)] -> Personagem
aplicarColecionavel jogador [] = jogador
aplicarColecionavel jogador (c@(colecionavel,(x,y)):t)
              | interseta (hitboxPersonagem jogador) (hitboxUnit (x,y)) = aplicarColecionavel (efeitoColecionavel jogador c) t
              | otherwise                                               = aplicarColecionavel jogador t


-- | A função @recolheColecionavelJogo@ torna possível o encadeamento das funções definidas anteriormente para a interação com colecionaveis dentro do mapa.
recolheColecionavelJogo :: Jogo -> Jogo
recolheColecionavelJogo jogo =
                   Jogo { 
                        mapa = mapa jogo,
                        inimigos = inimigos jogo,
                        colecionaveis = colecionaveisDisponiveis (jogador jogo) (colecionaveis jogo),
                        jogador = aplicarColecionavel (jogador jogo) (colecionaveis jogo),
                        pausa = False
                        }



-- | A função @countdownTempoMartelo@ desconta, a cada momento, o tempo útil de utilização do colecionável Martelo.
countdownTempoMartelo :: Personagem -> Tempo -> Personagem
countdownTempoMartelo jogador@Personagem{aplicaDano = (_, tRestante)} tempo 
            | tRestante > 0 = jogador {aplicaDano = (True, tRestante - tempo)}
            | otherwise     = jogador {aplicaDano = (False, 0)}


-- | A função @countdownTempoMarteloJogo@ fornece ao Jogo a capacidade de cronometrar a durabilidade do colecionável Martelo.
countdownTempoMarteloJogo :: Tempo -> Jogo -> Jogo
countdownTempoMarteloJogo t jogo = 
                         Jogo {
                              mapa = mapa jogo,
                              inimigos = inimigos jogo,
                              colecionaveis = colecionaveis jogo,
                              jogador = countdownTempoMartelo (jogador jogo) t,
                              pausa = False
                              }





---CONFIRMAR SE VAMOS TER PONTUACAO
-- | A função @bonusPontos@ atribui 200 pontos à cotação do jogador se esta atingir os 5 pontos.
bonusPontos :: Personagem -> Personagem
bonusPontos jogador
          | (pontos jogador * 100) == 500 = jogador { pontos = pontos jogador + 200}
          | otherwise             = jogador

-- | A função @bonusPontosJogo@ aplica ao Jogo uma função que dá boost na pontuação do jogador.
bonusPontosJogo :: Jogo -> Jogo
bonusPontosJogo jogo = 
                   Jogo {
                        mapa = mapa jogo,
                        inimigos = inimigos jogo,
                        colecionaveis = colecionaveis jogo, 
                        jogador = bonusPontos (jogador jogo),
                        pausa = False
                        }



----------- 6 ------------


-- | A função @pisarEmAlcapao@ modifica o mapa quando um jogador pisa num bloco alçapão.
jogadorPisaAlcapao :: Mapa -> Personagem -> Mapa
jogadorPisaAlcapao m@(Mapa a b blocos) jogador
          | blocoAbaixo (x,y) m == Alcapao = Mapa a b $ atualizarMapa blocos (floor x, floor y)
          | otherwise = Mapa a b blocos
     where (x,y) = posicao jogador

-- | A função @atualizarMapa@ modifica o mapa alterando uma linha 'x' em espícfico. Usamos take e drop para dividir a matriz em duas partes na posição 'x'.
atualizarMapa :: [[Bloco]] -> (Int, Int) -> [[Bloco]]
atualizarMapa blocos (x, y) = take x blocos ++ [atualizarLinha (blocos !! x) y] ++ drop (x + 1) blocos


-- | A função @atualizarLinha@ atualiza uma linha especifica na matriz dividindo a lista em duas partes na posição 'y' e substitui 'y' por Vazio.
atualizarLinha :: [Bloco] -> Int -> [Bloco]
atualizarLinha linha y = take y linha ++ [Vazio] ++ drop (y + 1) linha

-- | A função @jogadorPisaAlcapaoJogo@ torna possível o encadeamento das funções definidas anteriormente para a interação com colecionaveis dentro do mapa.
jogadorPisaAlcapaoJogo :: Jogo -> Jogo
jogadorPisaAlcapaoJogo j = 
               Jogo {
                    mapa = jogadorPisaAlcapao (mapa jogo)(jogador jogo), 
                    inimigos = inimigos jogo, 
                    colecionaveis = colecionaveis jogo,
                    jogador = jogador jogo,
                    pausa = False
                    }




{-
pisarAlcapao :: Mapa -> Personagem -> Bloco -> Bloco
pisarAlcapao m Personagem{tipo = jogador, posicao = (x,y)} bloco 
      | blocoAbaixo (floor x,floor y) m  == Alcapao = Vazio
      | otherwise                                   = bloco


aplicaEAlcapaoMapa :: Mapa -> Mapa
aplicaEAlcapaoMapa m@(Mapa pi d blocos) p = undefined
           -- map (\b -> pisarAlcapao m p b) blocos
           -}

{-
jogadorPisaAlcapao :: Personagem -> Mapa -> Mapa
jogadorPisaAlcapao jogador (Mapa x y matrizDeBlocos) =
    Mapa x y (map (\linha -> map (\bloco -> jog jogador bloco) linha) matrizDeBlocosComPosicoes)
    where
        matrizDeBlocosComPosicoes =
            zipWith (\linha y -> zipWith (\bloco x -> ((x,y), bloco)) linha [0..]) matrizDeBlocos [0..]


{-| A função jogadorPisaAlcapao utiliza a função auxiliar aplicaEfeitoAlcapao, que faz desaparecer o alçapão, quando o jogador o pisa.
 -}

jogadorPisaAlcapao :: Personagem -> Mapa -> Mapa
jogadorPisaAlcapao jogador (Mapa x y matrizDeBlocos) = Mapa x y (map (\linha -> map (\bloco -> aplicaEfeitoAlcapao jogador bloco) linha) matrizDeBlocosComPosicoes)
                                            where
                                                matrizDeBlocosComPosicoes = zipWith (\linha y -> zipWith (\bloco x -> ((x,y), bloco)) linha [0..]) matrizDeBlocos [0..]
{-| A função jogadorPisaAlcapao utiliza a função auxiliar aplicaEfeitoAlcapao, que faz desaparecer o alçapão, quando o jogador o pisa.
 -}

aplicaEfeitoAlcapao :: Personagem -> ((Int,Int), Bloco) -> Bloco
aplicaEfeitoAlcapao jogador ((x,y), bloco) | bloco == Alcapao && y == yinferior1 && x == xinferior1 = Vazio
                                           | bloco == Alcapao && y == yinferior2 && x == xinferior2 = Vazio
                                           | otherwise = bloco
                                           where
                                                ((xi,yi),(xs,ys)) = hitboxPersonagem jogador
                                                xinferior1 = floor xs
                                                yinferior1 = floor ys
                                                xinferior2 = floor xi
                                                yinferior2 = floor ys

-}

-------------- 7 -------------------

--------INIMIGO
-- | A função @inverterDirecaoInimigo@ altera a direçao do inimigo.
inverterDirecaoInimigo :: Personagem -> Personagem
inverterDirecaoInimigo inimigo = inimigo {direcao = case direcao inimigo of
              Este  -> Oeste
              Oeste -> Este
              _     -> direcao inimigo}

-- | A função @testaVazioInimigos@ define os limites que o inimigo pode percorrer quando está sob uma plataforma.
testaInimigosEmVazio :: Mapa -> Personagem -> Bool
testaInimigosEmVazio m inimigo =  direcao inimigo == Oeste && blocoAbaixo (x-1,y) m == Vazio || 
                                  direcao inimigo == Este  && blocoAbaixo (x+1,y) m == Vazio 
                        where (x,y) = posicao inimigo


-- | A função @atualizaPosicaoInimigo@ garante que a posicao dos inimigos é alterada ao longo do tempo tendo em conta a velocidade a que circula.
atualizaPosicaoInimigo :: Tempo -> Personagem -> Personagem
atualizaPosicaoInimigo t inimigo = inimigo {posicao = (x + vx * t, y)}
  where (x,y) = posicao inimigo
        (vx,_) = velocidade inimigo


-- | A função @restricoesMovimentoInimigo@ altera a direção do inimigo quando este colide com os limites do mapa, de qualquer bloco ou quando este está na extremidade de uma plataforma.
restricoesMovimentoInimigo :: Mapa -> Tempo-> Personagem -> Personagem
restricoesMovimentoInimigo mapa t inimigo
            | colisoesParede mapa inimigo || testaInimigosEmVazio mapa inimigo = inimigo{posicao = posicao $ atualizaPosicaoInimigo t inimigo, 
                                                                                         velocidade = ((-1)*vx,vy),
                                                                                         direcao = direcao $ inverterDirecaoInimigo inimigo }
            | otherwise                                                        = atualizaPosicaoInimigo t inimigo
                    where (vx,vy) = velocidade inimigo

-- | A função @limitacoesInimigos@ aplica a todos os inimigos do jogo as restriçoes indicadas acima.
limitacoesInimigos :: Tempo -> Mapa -> [Personagem] -> [Personagem]
limitacoesInimigos t m l = map (restricoesMovimentoInimigo m t) l


-- | A função @alteraPosicoesColecionaveisJogo@ torna possível o encadeamento das funções definidas anteriormente para a movimentação dos inimigos pelo mapa.
limitacoesInimigosJogo :: Tempo -> Jogo -> Jogo
limitacoesInimigosJogo t jogo = 
                         Jogo {
                              mapa = mapa jogo, 
                              inimigos = limitacoesInimigos t (mapa jogo) (inimigos jogo), 
                              colecionaveis = colecionaveis jogo, 
                              jogador = jogador jogo,
                              pausa = False
                              }



---------PERSONAGEM
-- | A função @movimentaJogador@ determina posicao do jogador a cada momento do jogo tendo em conta a velocidade a que circula.
movimentaJogador :: Mapa -> Tempo -> Personagem -> Personagem
movimentaJogador mapa@(Mapa _ _ bloco) t jogador  = jogador {posicao = (x + vx * t, y + vy * t)}
     where  (x,y) = posicao jogador
            (vx,vy) = velocidade jogador


-- | A função @movimentaJogadorJogo@ torna possivel o encadeamento das funções definidas anteriormente para a atualização do movimento go jogador.
movimentaJogadorJogo :: Tempo -> Jogo -> Jogo
movimentaJogadorJogo t jogo = 
                         Jogo {
                              mapa = mapa jogo, 
                              inimigos = inimigos jogo, 
                              colecionaveis = colecionaveis jogo, 
                              jogador = movimentaJogador (mapa jogo) t (jogador jogo),
                              pausa = False
                              }




------- CENTRA ESCADA ------

-- | A função @atualizaEmEscada@ atualiza o parâmetro emEscada do jogador.
atualizaEmEscadaJogador :: Mapa -> Personagem -> Personagem
atualizaEmEscadaJogador m@(Mapa _ _ blocos) jogador
            | encontraBloco (x,y) blocos == Escada = jogador {emEscada = True }
            | otherwise                            = jogador {emEscada = False}
        where (x,y) = posicao jogador

-- | A  função @atualizaEmEscada@ 
atualizaEmEscadaJogadorJogo :: Jogo -> Jogo
atualizaEmEscadaJogadorJogo jogo = 
                         Jogo {
                              mapa = mapa jogo, 
                              inimigos = inimigos jogo, 
                              colecionaveis = colecionaveis jogo, 
                              jogador = atualizaEmEscadaJogador (mapa jogo) (jogador jogo),
                              pausa = False
                              }


-- | A função @centroEscadaMaisProximo@ devolve a posicao do centro da escada mais próxima da posicao do jogador.
centroEscadaMaisProximo :: Mapa -> Posicao -> Posicao
centroEscadaMaisProximo m@(Mapa _ _ blocos) (x,y) = minimumBy (comparing distancia) escadas
        where escadas = detetaEscadas (0.5,0.5) blocos
              distancia (x', y') = sqrt ((x' - x)^2 + (y' - y)^2)

-- | A função @centraJogadorEmEscada@ adapta a posicao do jogador para que, quando este está numa escada, fique centrado nesta.
centraJogadorEmEscada :: Mapa -> Personagem -> Personagem
centraJogadorEmEscada mapa jogador
            | emEscada jogador && snd(velocidade jogador) == 0 = jogador {posicao = (xc-0.5, yc)}
            | emEscada jogador && snd(velocidade jogador) == 0 = jogador {posicao = (xc-0.5, yc)}
            | otherwise        = jogador
    where (x,y) = posicao jogador
          (l,a) = tamanho jogador
          (xc,yc) = centroEscadaMaisProximo mapa (x,y)

-- | A função @centraJogadorEmEscadaJogo@ torna possível o encadeamento das funções definidas anteriormente para a centralização do jogador em relação à escada.
centraJogadorEmEscadaJogo :: Jogo -> Jogo
centraJogadorEmEscadaJogo jogo =
                         Jogo {
                              mapa = mapa jogo, 
                              inimigos = inimigos jogo, 
                              colecionaveis = colecionaveis jogo, 
                              jogador = centraJogadorEmEscada (mapa jogo) (jogador jogo),
                              pausa = False
                              }



------ ALEATORIOS -------

s :: StdGen
s = mkStdGen 5

-- | A função @geraListaPosicoesAleatorias@ gera pares de coordenadas aleatórias dentro do retângulo definido pelas dimensões do mapa. A semente do gerador é dividida para garantir novas sequências aleatórias a cada chamada. 
geraListaPosicoes :: Mapa -> [Posicao]
geraListaPosicoes m = take 15 $ zip (randomRs (x1, x2) s1) (randomRs (y1, y2) s2)
            where ((x1, y1), (x2, y2)) = hitboxmapa m
                  (s1, s2) = split s

-- | A função @validaNovasPosicoes@ averigua se as posicoes geradas aleatoriamente são válidas.
validaNovasPosicoes :: Mapa -> [Posicao] -> [Posicao]
validaNovasPosicoes m@(Mapa _ _ blocos) l = filter (\p -> encontraBloco p blocos == Vazio) l

-- | A função @posicoesGeradas@ cria uma lista das posições aleatórias geradas que são válidas para as posições dos colecionáveis.
posicoesGeradas :: Mapa -> [Posicao]
posicoesGeradas m = validaNovasPosicoes m (geraListaPosicoes m)


-- | A função @listaColecionaveis@ separa a lista de colecionaveis e cria um par com a lista do colecionáveis e a lista das suas posições.
listaColecionaveis :: [(Colecionavel, Posicao)] -> ([Colecionavel],[Posicao])
listaColecionaveis l = unzip l

-- | A função @alteraPosicoesColecionaveis@ cria uma lista com os colecionaveis do jogo e as novas posições geradas para os mesmos.
alteraPosicoesColecionaveis :: Mapa -> [(Colecionavel, Posicao)] -> [(Colecionavel, Posicao)]
alteraPosicoesColecionaveis m l = zip col (posicoesGeradas m)
                        where (col,pos) = listaColecionaveis l

-- | A função @alteraPosicoesColecionaveisJogo@ torna possível o encadeamento das funções definidas anteriormente para a aleatoriedade do posicionamento dos colecionaveis no jogo.
alteraPosicoesColecionaveisJogo :: Jogo -> Jogo
alteraPosicoesColecionaveisJogo jogo =
                         Jogo {
                              mapa = mapa jogo, 
                              inimigos = inimigos jogo, 
                              colecionaveis = alteraPosicoesColecionaveis (mapa jogo) (colecionaveis jogo), 
                              jogador = jogador jogo,
                              pausa = False
                              }





------ pontuaçao
------ stop      
------ IMAGEM PARA PERDER VIDA
------ colisoesLaterais do jogador 
------ game over           DONE
------   countdown       DONE


-----------------------------------------------------------------------------------------------------------------



-----------------------------------------------------------------------------------------------------------------
