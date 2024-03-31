module Fliperama.Services.Session (getCurrentPlayerUserName, setSessionData) where

import qualified Fliperama.Repositories.Session as SessionRepositories

getCurrentPlayerUserName :: IO String
getCurrentPlayerUserName =  do
    currentSessionPlayer <- SessionRepositories.getCurrentSessionPlayer
    return currentSessionPlayer

setSessionData :: String -> String -> IO()
setSessionData username msg =
    SessionRepositories.saveSessionData (SessionRepositories.buildSessionData username msg)