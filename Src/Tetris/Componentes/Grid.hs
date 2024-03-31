module Tetris.Componentes.Grid where

import Graphics.Gloss.Interface.IO.Game

import Graphics.Gloss

renderizaGrid :: [[Int]] -> (Float, Float) -> Picture
renderizaGrid grid (xP, yP) = translate xP yP $ pictures $ map renderizaLinha $ zip  grid [0..]
  where
    renderizaLinha (row, y) = pictures $ map (\(cell, x) -> renderizaCelula cell x y) $ zip row [0..]
    renderizaCelula cell x y = translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) $ color (colorForCell cell) $ rectangleSolid cellSize cellSize
    cellSize = 30
    colorForCell cell
        | (cell `mod` 10) == 0 = black
        | (cell `mod` 10) == 1 = blue
        | (cell `mod` 10) == 2 = orange
        | (cell `mod` 10) == 3 = yellow
        | (cell `mod` 10) == 4 = dark blue
        | (cell `mod` 10) == 5 = green
        | (cell `mod` 10) == 6 = violet
        | (cell `mod` 10) == 7 = red
        | otherwise = error ("Objeto não cadastrado")
