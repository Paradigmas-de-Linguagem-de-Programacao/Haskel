module Componentes.Pecas where

import Graphics.Gloss

import Componentes.Grid(renderizaGrid)

renderizaProximaPeca :: [[Int]] -> (Float, Float) -> (Float, Float) -> String -> Picture
renderizaProximaPeca grid (xTexto, yTexto) (xGrid, yGrid) texto = pictures [proximaPeca, proximo']
  where
    proximaPeca = renderizaGrid (reverse grid) (xGrid, yGrid)
    proximo' = translate xTexto yTexto $ scale 0.2 0.2 $ color white $ text texto