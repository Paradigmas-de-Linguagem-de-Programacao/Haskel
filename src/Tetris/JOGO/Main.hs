module Main where

import Data.Char
import Graphics.Gloss.Interface.IO.Game
import Util.LimparJogo
import Util.ControleJogo
import Util.Estado (
  Estado(..), Peca(..), geraPeca, geraEstadoInicial, atribuicaoPeca, verificaAtribuicaoPeca, verificaRotacao, rotacionaPeca, limpaPeca
  )
import Componentes.Grid
import Componentes.Texto
import Componentes.Pecas

resolucao :: (Int, Int)
resolucao = (1200,800)

posicaoinicial :: (Int, Int)
posicaoinicial = (10,10)

main :: IO ()
main = playIO (InWindow "Tetris" resolucao posicaoinicial)
              (light black)
              1
              geraEstadoInicial
              renderizacao
              inputTeclado
              atualizaTempo

renderizacao :: Estado -> IO Picture
renderizacao estado = return $ pictures [
  caixaGrid, caixaNivel, caixaLinhas, caixaPontuacao, 
  caixaTempo ,caixaProximaPeca, caixaPerdeu, 
  caixaComandoA, caixaComandoD, caixaComandoS, caixaComandoR, 
  caixaComandoK, caixaComandoL, caixaComandoX]
  where
    caixaGrid = renderizaGrid (grid estado) (-150, -300)
    caixaNivel = renderizaTexto "Nivel" (show (nivel estado)) (150, -200, 0.2, 0.2)
    caixaLinhas = renderizaTexto "Linhas" (show (linhas estado)) (150, -250, 0.2, 0.2)
    caixaPontuacao = renderizaTexto "Pontuacao" (show (pontuacao estado)) (150, -150, 0.2, 0.2)
    caixaTempo = renderizaTexto "Tempo" (show (tempo estado)) (150, -300, 0.2, 0.2)
    caixaProximaPeca = renderizaProximaPeca (head (formatosPeca (proximaPeca estado))) (150, 10) (180, -60) "Proxima"
    caixaComandoA = renderizaTexto "seta pra esquerda mover pra esquerda" "" (-420, 250, 0.1, 0.1)
    caixaComandoD = renderizaTexto "seta pra direita mover pra direita" "" (-420, 200, 0.1, 0.1)
    caixaComandoS = renderizaTexto "seta pra baixo mover pra Baixo" "" (-420, 150, 0.1, 0.1)
    caixaComandoR = renderizaTexto "R - reniciar Jogo" "" (-420, 100, 0.1, 0.1)
    caixaComandoK = renderizaTexto "Z - Rotacionar anti-horario" "" (-420, 50, 0.1, 0.1)
    caixaComandoL = renderizaTexto "X - Rotacionar horario" "" (-420, 1, 0.1, 0.1)
    caixaComandoX = renderizaTexto "espaco - Jogar a peca pra baixo" "" (-420, -50, 0.1, 0.1)
    caixaPerdeu = if jogoAcabou estado then renderizaTexto "Voce" "Perdeu" (150, 100, 0.2, 0.2) else renderizaTexto "" "" (150, -350, 0.2, 0.2)

inputTeclado :: Event -> Estado -> IO Estado
inputTeclado (EventKey (Char t) Down _ _ ) estado
  | tecla /= 'r' &&  jogoAcabou estado = return estado
  | tecla == 'z' = return $
      if verificaRotacao (grid estado) (atualPeca estado) True
      then rotacionaPeca estado True
      else estado
  | tecla == 'x' = return $
      if verificaRotacao (grid estado) (atualPeca estado) False
      then rotacionaPeca estado False
      else estado
  | tecla == 'r' = return geraEstadoInicial
  | otherwise = return estado
  where
    tecla = toLower t
inputTeclado (EventKey (SpecialKey tecla) Down _ _) estado 
  | tecla == KeyLeft = return $ if verificaShiftEsquerda (grid estado)
      then estado {
        grid = shiftEsquerda (grid estado),
        atualPeca = (atualPeca estado) {
        coordenadas = ((x1-1, y1), (x2-1,y2))
        }
        }
    else estado
  | tecla == KeyRight = return $ if verificaShiftDireita (grid estado)
      then estado {
        grid = shiftDireita (grid estado),
        atualPeca = (atualPeca estado) {
        coordenadas = ((x1+1, y1), (x2+1,y2))
        }
        }
    else estado
  | tecla == KeyDown = return $ 
      if verificaShiftBaixo (grid estado)
      then estado {
        grid = shiftBaixo (grid estado),
        pontuacao = pontuacao estado + 1 * nivel estado,
        atualPeca = (atualPeca estado) {
        coordenadas = ((x1, y1-1), (x2,y2-1))
        }
        }
      else estado
  | tecla == KeySpace = descerCompleto estado
    where 
       ((x1, y1), (x2,y2)) = coordenadas (atualPeca estado)
       descerCompleto estado = 
        if verificaShiftBaixo (grid estado) 
        then descerCompleto estado {
          grid = shiftBaixo (grid estado),
          pontuacao = pontuacao estado + 5 * nivel estado,
          atualPeca = (atualPeca estado) {
          coordenadas = ((x1, y1-1), (x2,y2-1))
          }
        } 
        else atualizaTempo 1 estado

inputTeclado _ estado = return estado

atualizaTempo :: Float -> Estado -> IO Estado
atualizaTempo 1 estado = mudaEstado
  where
    ((x1, y1), (x2,y2)) = coordenadas (atualPeca estado)
    jogoAcabou = not $ verificaAtribuicaoPeca (grid estado) (formatosPeca (proximaPeca estado) !! atualEstado (proximaPeca estado))
    (gridLimpa, qtdLinhasLimpas) = clearGame (grid estado)
    mudaEstado
      | verificaShiftBaixo (grid estado) = return estado { 
        grid = shiftBaixo (grid estado), 
        tempo = tempo estado + 1,
        atualPeca = (atualPeca estado) {
        coordenadas = ((x1, y1-1), (x2,y2-1))
        }}
      | jogoAcabou = return estado {
    jogoAcabou = jogoAcabou
 }
      | otherwise = return estado {
        grid = atribuicaoPeca (congelarTudo gridLimpa) (head (formatosPeca (proximaPeca estado))),
        atualPeca = proximaPeca estado,
        proximaPeca = geraPeca ((qualPeca (proximaPeca estado) + (1 + nivel estado * linhas estado)) `mod` 7),
        tempo = tempo estado + 1,
        pontuacao = pontuacao estado + qtdLinhasLimpas * nivel estado,
        linhas = linhas estado + qtdLinhasLimpas,
        nivel = (linhas estado `div` 10) + 1
        } 
atualizaTempo _ estado = return estado 