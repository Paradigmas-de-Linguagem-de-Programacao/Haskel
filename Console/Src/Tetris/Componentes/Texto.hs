module Src.Tetris.Componentes.Texto where

import Graphics.Gloss.Interface.IO.Game

renderizaTexto :: String -> String -> (Float, Float, Float, Float) -> Picture
renderizaTexto text1 text2 (xPosition, yPosition, altura, comprimento) = 
    Translate xPosition yPosition $ Color white $ Scale altura comprimento $ Text (text1 ++ " " ++text2)