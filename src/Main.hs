module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Color
import System.Random
import LI12324
import Data.Maybe
import Graphics.Gloss.Juicy
import GHC.Float
import Tarefa4
import Tarefa1
import Tarefa0
import Tarefa2
import Tarefa3

data Imagem = Play | Exit | Stair | Trapdoor | Plataform | Empty | ButaoTutorial | ImagemTutorial | Mario | Estrela | FotoMoeda deriving (Show, Eq)

type Imagens = [(Imagem, Maybe Picture)]

data MenuInicialOpcoes = Jogar | Sair | Tutorial deriving (Show, Eq)

data Modo = MenuInicial MenuInicialOpcoes | EmJogo | TutorialFoto deriving (Show, Eq)

data Estado = Estado
  { jogo :: Jogo,  -- Assuming Jogo is defined somewhere else in your code
    imagens :: Imagens,
    modo :: Modo,
    tempo :: Float
  }

getImagem :: Imagem -> Imagens -> Picture
getImagem k d = case lookup k d of
  Just (Just img) -> img
  _               -> Blank

desenha :: Estado -> IO Picture
desenha e@(Estado (Jogo mapa _ _ _ pausa) imgs (MenuInicial opcaoSelecionada) _) =
      return $ Pictures [ if opcaoSelecionada == Jogar
          then Scale 2 2 (Translate 0 100 (getImagem Play imgs))
          else Translate 0 100 (getImagem Play imgs)
      , if opcaoSelecionada == Sair
          then Scale 2 2 (Translate 0 (0) (getImagem Exit imgs))
          else Translate 0 0 (getImagem Exit imgs)
      , if opcaoSelecionada == Tutorial
          then scale 2 2 (Translate 0 (-100) (getImagem  ButaoTutorial imgs))
          else  Translate 0 (-150) (getImagem  ButaoTutorial imgs)
      ]
desenha e@(Estado (Jogo mapa@(Mapa (pos,_) _ _) _ _ jogador pausa) imgs (EmJogo) _) = return $ Pictures [mapaToPicture imgs mapa, desenhaJogador e, rederizaAjudas e, desenhaColecionaveis e]
desenha e@(Estado _ imgs (TutorialFoto) _) = return $ getImagem ImagemTutorial imgs

reage :: Event -> Estado -> IO Estado
reage (EventKey (SpecialKey KeyDown) Down _ _) e@(Estado _ (imgs) (MenuInicial Jogar) _) =
  return e {modo = MenuInicial Sair}
reage (EventKey (SpecialKey KeyDown) Down _ _) e@(Estado _ (imgs) (MenuInicial Sair) _) =
  return e {modo = MenuInicial Tutorial}
reage (EventKey (SpecialKey KeyUp) Down _ _) e@(Estado _ (imgs) (MenuInicial Tutorial) _) =
  return e {modo = MenuInicial Sair}
reage (EventKey (SpecialKey KeyUp) Down _ _) e@(Estado _ (imgs) (MenuInicial Sair) _) =
  return e {modo = MenuInicial Jogar}
reage (EventKey (SpecialKey KeyEnter) Down _ _) e@(Estado _ _ (MenuInicial Jogar) _) =
  return e {modo = EmJogo}
reage (EventKey (SpecialKey KeyEnter) Down _ _) e@(Estado _ _ (MenuInicial Sair) _) =
  error "Sair selected" 
reage (EventKey (SpecialKey KeyEnter) Down _ _) e@(Estado _ (imgs) (MenuInicial Tutorial) _) = 
  return e {modo = TutorialFoto}
reage (EventKey (SpecialKey KeyEsc) Down _ _) e@(Estado _ (imgs) (TutorialFoto) _) = 
  return e {modo = MenuInicial Jogar}
reage _ e = return e

janela :: Display
janela = InWindow "" (1920, float2Int alturaJanela) (0,0)

corFundo :: Color
corFundo = greyN 1.0

frameRate :: Int
frameRate = 60

carregarImagens :: IO Estado
carregarImagens = do
  play <- loadJuicy "src/images/PLAY.bmp"
  sair <- loadJuicy "src/images/PLAY.bmp"
  plataform <- loadJuicy "src/blocos/blocoPlataforma.bmp"
  stair <- loadJuicy "src/blocos/escadasBloco.bmp"
  trapdoor <- loadJuicy "src/blocos/blocoAlcapao.bmp"
  empty <- loadJuicy "src/blocos/blocoVazio.bmp"
  tutorial <- loadJuicy "src/images/tutorial.bmp"
  mario <- loadJuicy "src/images/marioDir2.png"
  imagemtutorial <- loadJuicy "src/images/Tutorial.png"
  moeda <- loadJuicy "src/images/moeda.png"
  estrela <- loadJuicy "src/images/ESTRELA.png"
  let imgs = [(Play, play), (Exit, sair),(Stair,stair),(Plataform,plataform),(Trapdoor,trapdoor),(Empty,empty),(ButaoTutorial,tutorial),(Mario,mario),(ImagemTutorial,imagemtutorial),(Estrela,estrela),(FotoMoeda,moeda)]
      jogoInicial = Jogo
               { mapa = mapa01,
                 inimigos = [inimigoParado],
                 colecionaveis = [(Moeda,posicaoMoeda)],
                 jogador = jogadorParado,
                 pausa = False
               }
  return (Estado {jogo = jogoInicial, imagens = imgs, modo = MenuInicial Jogar, tempo = 0})

main :: IO ()
main = do
  imgs <- carregarImagens
  playIO
   janela
   corFundo
   frameRate
   imgs
   desenha
   handleEvent
   atualizaTempo
   


-- Função para converter um Bloco em uma Picture
blocoToPicture :: Imagens -> Bloco -> Picture
blocoToPicture imgs Escada = (getImagem Stair imgs)
blocoToPicture imgs Plataforma = (getImagem Plataform imgs)
blocoToPicture imgs Alcapao = (getImagem Trapdoor imgs)
blocoToPicture imgs Vazio = (getImagem Empty imgs)

-- Função para converter um Mapa em uma Picture
mapaToPicture :: Imagens -> Mapa -> Picture
mapaToPicture imgs (Mapa (pos, dir) _ blocos) =
  Pictures [Translate (double2Float (x - float2Double meioLarguraMapa) * tamanhoBloco + tamanhoBloco / 2) (double2Float (y - float2Double meioAlturaMapa) * tamanhoBloco + tamanhoBloco / 2) (blocoToPicture imgs bloco) | (y, row) <- zip [0 ..] (reverse blocos), (x, bloco) <- zip [0 ..] (row)]
  where
    larguraMapa = fromIntegral (length (head blocos))
    alturaMapa = fromIntegral (length blocos)
    tamanhoBloco = min (larguraJanela / larguraMapa) (alturaJanela / alturaMapa)
    meioLarguraMapa = larguraMapa / 2
    meioAlturaMapa = alturaMapa / 2



converteX :: Double -> (Double,Double) -> Float
converteX x (a,b) = (-larguraJanela / 2) + (double2Float x *  int2Float pixeis + double2Float a * int2Float pixeis)


converteY :: Double -> (Double,Double) -> Float
converteY y (a,b) = (alturaJanela / 2) - (double2Float y *  int2Float pixeis + double2Float b * int2Float pixeis)

larguraMapa = length (head bloco)
alturaMapa = length bloco

larguraJanela = 972
alturaJanela = 1080
bloco = [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Alcapao, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Escada, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Alcapao, Alcapao, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Alcapao, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Alcapao, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Alcapao, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma],
  [Plataforma, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Alcapao, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Escada, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
  [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]

desenhaJogador :: Estado -> Picture
desenhaJogador e = case direcaoJogador of
    Este -> Translate (converteX x t - int2Float pixeis / 2) (converteY y t+int2Float pixeis) (scale 1.8 1.8 (getImagem Mario imagem))
    Oeste -> Translate (converteX x t - int2Float pixeis) (converteY y t +int2Float pixeis) (scale (-1.8) 1.8 (getImagem Mario imagem))
    Norte -> Translate (converteX x t - int2Float pixeis) (converteY y t+int2Float pixeis) (scale 1.8 1.8 (getImagem Mario imagem))
    Sul -> Translate (converteX x t - int2Float pixeis) (converteY y t +int2Float pixeis) (scale (-1.8) 1.8 (getImagem Mario imagem))
    where (x,y) = posicao (jogador (jogo e)) 
          direcaoJogador = direcao (jogador (jogo e))
          imagem = imagens e
          t = tamanho (jogador (jogo e))

desenhaColecionaveis :: Estado -> Picture
desenhaColecionaveis e = Pictures $ map (uncurry (desenhaColecionavel e)) (colecionaveis (jogo e))

desenhaColecionavel :: Estado -> Colecionavel -> Posicao -> Picture
desenhaColecionavel e Moeda posicaoMoeda = scale 0.5 0.5 $ Translate (double2Float (fst posicaoMoeda)) (double2Float $ snd posicaoMoeda) (getImagem FotoMoeda imagem)
  where imagem = imagens e
desenhaColecionavel e Martelo posicaoMartelo = Translate (double2Float $ fst posicaoMartelo) (double2Float $ snd posicaoMartelo) (getImagem Estrela imagem)
  where imagem = imagens e

posicaoMoeda :: Posicao
posicaoMoeda = (8,30)

posicaoMartelo :: Posicao
posicaoMartelo = (3, 4)


eventosJogo :: Event -> Estado -> Estado
eventosJogo (EventKey (SpecialKey KeyRight) Down _ _) e = e {jogo = atualiza [Nothing] (Just AndarDireita) (jogo e)} 
eventosJogo (EventKey (SpecialKey KeyRight) Up _ _) e = e {jogo = atualiza [Nothing] (Just Parar) (jogo e)}
eventosJogo (EventKey (SpecialKey KeyLeft) Down _ _) e = e {jogo = atualiza [Nothing] (Just AndarEsquerda) (jogo e)}
eventosJogo (EventKey (SpecialKey KeyLeft) Up _ _) e = e {jogo = atualiza [Nothing] (Just Parar) (jogo e)}
eventosJogo (EventKey (SpecialKey KeyUp) Down _ _) e = e {jogo= atualiza [Nothing] (Just Subir) (jogo e)}
eventosJogo (EventKey (SpecialKey KeyUp) Up _ _) e = e {jogo= atualiza [Nothing] (Just Parar) (jogo e)}
eventosJogo (EventKey (SpecialKey KeyDown) Down _ _) e = e {jogo= atualiza [Nothing] (Just Descer) (jogo e)}
eventosJogo (EventKey (SpecialKey KeyDown) Up _ _) e = e {jogo = atualiza [Nothing] (Just Parar) (jogo e)}
eventosJogo (EventKey (SpecialKey KeySpace) Down _ _) e = e {jogo = atualiza [Nothing] (Just Saltar) (jogo e)}
--eventosJogo (EventKey (SpecialKey KeySpace) Up _ _) e = e {jogo = atualiza [Nothing] (Just Parar) (jogo e)}
eventosJogo (EventKey (Char 'p') Down _ _) e@(Estado _ (imgs) (EmJogo) _) = e {jogo = (jogo e) {pausa = if pausa (jogo e) then False else True } }
eventosJogo (EventKey (SpecialKey KeyEsc) Down _ _) e = e {modo = MenuInicial Jogar}
eventosJogo _ e = e

handleEvent :: Event -> Estado -> IO Estado
handleEvent e estado
   | modo estado == EmJogo = return $ eventosJogo e estado 
   | otherwise = reage e estado


pixeis :: Int
pixeis = 27



jogadorParado =
  Personagem
    { velocidade = (0,0),
      tipo = Jogador,
      posicao = (5.5,4.5),
      direcao = Este,
      tamanho = (0.8, 0.8),
      emEscada = False,
      ressalta = False,
      vida = 10,
      pontos = 0,
      aplicaDano = (False, 0)
    }

mapa01 :: Mapa
mapa01 =
  Mapa
    ((0,0), Este)
    (3,2.5)
    bloco

inimigoParado =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = Fantasma,
      posicao = (2.5, 7.6),
      direcao = Este,
      tamanho = (1, 1),
      emEscada = False,
      ressalta = True,
      vida = 1,
      pontos = 0,
      aplicaDano = (False, 0)
    }




hitBoxpic :: Hitbox -> Picture
hitBoxpic ((xi,yi),(xs,ys)) = color red $ rectangleWire (realToFrac ((xs* fromIntegral pixeis)-(xi* fromIntegral pixeis))) (realToFrac ((ys* fromIntegral pixeis)-(yi* fromIntegral pixeis)))
                            

ajudaDeJogadorPic :: Jogo -> Mapa -> Personagem -> Picture
ajudaDeJogadorPic jogo m personagem =  color blue $ textoPic (textoDePersonagem jogo m personagem)

textoPic :: [String] -> Picture
textoPic linhas =   pictures $ zipWith (\y linha -> scale 0.1 0.1 $ translate 0 y $ text linha) [0,-120..] linhas

textoDePersonagem :: Jogo -> Mapa -> Personagem-> [String]
textoDePersonagem jogo m@(Mapa _ _ bloco) personagem =
    [ 
        "Vida: " ++ show (vida personagem),
        "Pontos: " ++ show (pontos personagem),
        "Aplica Dano: " ++ show (aplicaDano personagem),
        "Direcao: " ++ show (direcao personagem),
        "Em Escada: " ++ show (emEscada personagem),
        "Ressalta: " ++ show (ressalta personagem),
        "Posicao: " ++ show (posicao personagem),
        "Tamanho: " ++ show (tamanho personagem),
        "Tipo: " ++ show (tipo personagem),
         "Velocidade: " ++ show (velocidade personagem),
        "x mod 32: " ++ show (floor (fst (posicao personagem)) `mod` 32),
        "hitbox: " ++ show (hitboxPersonagem personagem),
        "BlocoAbaixo:" ++ show (blocoAbaixo (posicao personagem) m),
        "Bloco:" ++ show (encontraBloco (posicao personagem) bloco),
        "colecionaveis" ++ show (colecionaveis jogo)
    ]

rederizaAjudas :: Estado -> Picture
rederizaAjudas estado  = 
                let 
                    ((xi,yi),(xs,ys)) = hitboxPersonagem (jogador (jogo estado))
                    (x,y) = posicao (jogador (jogo estado))
                    t = tamanho (jogador (jogo estado))

                in 
                    pictures [ 
                                translate (converteX x t) (converteY y t) $ hitBoxpic (hitboxPersonagem (jogador (jogo estado))),
                                translate (converteX 5 t) (converteY 5 t) $ ajudaDeJogadorPic (jogo estado) (mapa (jogo estado)) (jogador (jogo estado))
                             ]

atualizaTempo :: Float -> Estado -> IO Estado
atualizaTempo segundos estado@(Estado {modo = EmJogo}) = return estado { 
    jogo = jogoAtualizado,
    tempo = tempoTotalAtualizado   
    
    }
    where
        jogoAtualizado = movimenta 1 (realToFrac segundos) (jogo estado)
        tempoTotalAtualizado = tempo estado + segundos  
atualizaTempo _ estado = return estado
