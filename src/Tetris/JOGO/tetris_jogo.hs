module Main where

import Graphics.Gloss.Interface.IO.Game

-- Constantes:
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
              orange                                          -- Cor de fundo
              60                                              -- Atualizações por segundo
              initialModel                                   -- Estado inicial
              render                                         -- Função de renderização
              handleInput                                    -- Função de manipulação de entrada
              update                                         -- Função de atualização

-- Definição do estado inicial

initialModel :: [[Int]]
initialModel =  [ [ if b == 1 && a == 1 then 1 else 0 | a <- [1..colunas] ] | b <- [1..linhas] ]

-- Rendering function
render :: [[Int]] -> IO Picture
render grid = return $ pictures $ map renderRow $ zip grid [0..]
  where
    renderRow (row, y) = pictures $ map (\(cell, x) -> renderCell cell x y) $ zip row [0..]
    renderCell cell x y = translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) $ color (colorForCell cell) $ rectangleSolid cellSize cellSize
    cellSize = 30
    colorForCell cell
        | cell == 0 = white
        | otherwise = black

-- Função de manipulação de entrada
handleInput :: Event -> [[Int]] -> IO [[Int]]
handleInput _ = return
-- handleInput _ ( grid) =
--   let rabo = init grid             -- All rows except the last one
--       cabeca = last grid           -- The last row
--   in return $ (cabeca : rabo)  -- Prepend the last row to the beginning


-- Função de atualização (neste exemplo, não é necessária)
update :: Float -> [[Int]] -> IO [[Int]]
update _ = return