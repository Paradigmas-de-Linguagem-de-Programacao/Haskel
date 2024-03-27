module Componentes.Pontuacao where

import Graphics.Gloss.Interface.IO.Game

renderizaPontuacao :: String -> Picture
renderizaPontuacao text = Translate xPosition yPosition $ Color white $ Scale altura comprimento $ Text ("Pontuacao " ++ text)
  where
    xPosition = 150
    yPosition = -150
    altura = 0.2 
    comprimento = 0.2  