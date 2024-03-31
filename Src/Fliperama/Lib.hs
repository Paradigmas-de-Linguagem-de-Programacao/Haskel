module Src.Fliperama.Lib where

import qualified Src.Fliperama.Services.Actions as ActionServices
import qualified Src.Fliperama.Services.Session as SessionServices

getCurrentPlayerUsername :: IO String
getCurrentPlayerUsername = SessionServices.getCurrentPlayerUserName

