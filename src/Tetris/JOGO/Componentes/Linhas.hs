module Componentes.Linhas where

import Graphics.Gloss.Interface.IO.Game

renderizaLinhas :: String -> Picture
renderizaLinhas text = Translate xPosition yPosition $ Color white $ Scale altura comprimento $ Text ("Linhas " ++ text)
  where
    xPosition = 150
    yPosition = -250
    altura = 0.2 
    comprimento = 0.2  