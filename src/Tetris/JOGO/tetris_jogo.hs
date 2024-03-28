module Main where
import Graphics.Gloss.Interface.IO.Game
import Util.LimparJogo
import Util.ControleJogo
import Util.Estado (
  Estado(..), EstatisticaPeca(..), Peca(..), geraPeca, geraEstadoInicial, atribuicaoPeca
  )
import Componentes.Grid
import Componentes.Nivel 
import Componentes.Linhas
import Componentes.Pontuacao
import Componentes.ProximaPeca

resolucao :: (Int, Int)
resolucao = (1200,800)

posicaoinicial :: (Int, Int)
posicaoinicial = (10,10)

main :: IO ()
main = playIO (InWindow "Grid" resolucao posicaoinicial) 
              (light black)                                        
              24                                              
              estadoInicial                                  
              renderizacao                                         
              inputTeclado                                    
              atualizaTempo                                         


estadoInicial :: Estado
estadoInicial =  geraEstadoInicial

renderizacao :: Estado -> IO Picture
renderizacao estado = return $ pictures [caixaGrid, caixaNivel, caixaLinhas, caixaPontuacao, caixaProximaPeca]
  where
    caixaGrid = renderizaGrid (grid estado) (-150, -300)
    caixaNivel = renderizaNivel (show (nivel estado))
    caixaLinhas = renderizaLinhas (show (linhas estado))
    caixaPontuacao = renderizaPontuacao (show (pontuacao estado))
    caixaProximaPeca = renderizaProximaPeca (formatoPeca (proximaPeca estado))

inputTeclado :: Event -> Estado -> IO Estado
inputTeclado (EventKey (Char 'a') Down _ _) estado = return $ if verificaShiftEsquerda (grid estado) then estado { grid = shiftEsquerda (grid estado)} else estado
inputTeclado (EventKey (Char 's') Down _ _) estado = return $ if verificaShiftBaixo (grid estado) then estado { grid = shiftBaixo (grid estado)} else estado
inputTeclado (EventKey (Char 'd') Down _ _) estado = return $ if verificaShiftDireita (grid estado) then estado { grid = shiftDireita (grid estado)} else estado
inputTeclado _ estado = return estado

atualizaTempo :: Float -> Estado -> IO Estado
atualizaTempo t estado = if (round t) == 0 then error (show t) else return estado
  -- where 
  -- mudaEstado = if verificaShiftBaixo (grid estado) 
  --   then  return estado { grid = shiftBaixo (grid estado)}
  --   else  return estado { 
  --   grid = atribuicaoPeca (congelarTudo (grid estado)) (formatoPeca (proximaPeca estado)), 
  --   atualPeca = proximaPeca estado, 
  --   proximaPeca = geraPeca ((qualPeca (proximaPeca estado) + 1) `mod` 7)
  -- }