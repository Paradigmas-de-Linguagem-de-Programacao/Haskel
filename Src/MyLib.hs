module Src.MyLib where

import qualified Src.Tetris.Main (
    renderizacao, inputTeclado, atualizaTempo
    )

import qualified Src.Tetris.Util.Estado (geraEstadoInicial)

import qualified Src.Fmcc.Main (startFmcc)

import qualified Src.Fliperama.Services.Actions as ActionServices
import qualified Src.Fliperama.Services.Session as SessionServices

getCurrentPlayerUsername :: IO String
getCurrentPlayerUsername = SessionServices.getCurrentPlayerUserName