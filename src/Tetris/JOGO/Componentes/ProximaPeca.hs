module Componentes.ProximaPeca where

import Graphics.Gloss

import Componentes.Grid(renderizaGrid)

renderizaProximaPeca :: [[Int]] -> Picture
renderizaProximaPeca grid = pictures [proximaPeca, proximo']
  where
    proximaPeca = renderizaGrid (reverse grid) (180, -60)
    proximo' = translate 150 10 $ scale 0.2 0.2 $ color white $ text "Proxima" 