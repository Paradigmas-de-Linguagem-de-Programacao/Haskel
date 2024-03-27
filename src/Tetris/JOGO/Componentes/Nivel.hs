module Componentes.Nivel where

import Graphics.Gloss.Interface.IO.Game

renderizaNivel :: String -> Picture
renderizaNivel text = Translate xPosition yPosition $ Color white $ Scale altura comprimento $ Text ("Nivel " ++ text)
  where
    xPosition = 150
    yPosition = -200
    altura = 0.2 
    comprimento = 0.2  
