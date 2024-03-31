module Fliperama.Lib where

import qualified Services.Actions as ActionServices
import qualified Services.Session as SessionServices

getCurrentPlayerUsername :: IO String
getCurrentPlayerUsername = SessionServices.getCurrentPlayerUserName

