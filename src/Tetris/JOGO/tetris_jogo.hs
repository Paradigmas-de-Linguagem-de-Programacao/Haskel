module Main where

import Graphics.Gloss.Interface.IO.Game

import Util.LimparJogo

import Util.ControleJogo

import Util.Estado

import Componentes.Grid

linhas :: Int
linhas = 20

colunas :: Int
colunas = 10

resolucao :: (Int, Int)
resolucao = (1200,800)

posicaoinicial :: (Int, Int)
posicaoinicial = (10,10)

main :: IO ()
main = playIO (InWindow "Grid" resolucao posicaoinicial) -- Título da janela, tamanho da janela, posição inicial da janela
              (light black)                                          -- Cor de fundo
              60                                              -- Atualizações por segundo
              initialModel                                   -- Estado inicial
              render                                         -- Função de renderização
              handleInput                                    -- Função de manipulação de entrada
              update                                         -- Função de atualização

-- Definição do estado inicial

initialModel :: Estado
initialModel =  geraEstadoInicial

data Estado = Estado {
    grid :: [[Int]],
    nivel :: Int,
    pontuacao :: Int,
    atualPeca :: Peca,
    proximaPeca :: Peca,
    estatisticas :: [EstatisticaPeca]
} deriving (Show)

-- Rendering function
render :: Estado -> IO Picture
render estado = do 
  telaGrid <- renderizaGrid (grid estado)
  return $ pictures [telaGrid]

-- Função de manipulação de entrada
handleInput :: Event -> Estado -> IO Estado
handleInput _ = return
-- handleInput _ ( grid) =
--   let rabo = init grid             -- All rows except the last one
--       cabeca = last grid           -- The last row
--   in return $ (cabeca : rabo)  -- Prepend the last row to the beginning


-- Função de atualização (neste exemplo, não é necessária)
update :: Float -> Estado -> IO Estado
update _ = return