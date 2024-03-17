module Repositories.Session (saveSession, getSessionData) where

import System.IO
import Data.List.Split (splitOn)

saveSession :: String -> IO()
saveSession username = do
    let path = "Repositories/data/session.txt"
    let newData = username ++ "\n"
    appendFile path newData

getSessionData :: IO String
getSessionData = readFile "Repositories/data/session.txt"
