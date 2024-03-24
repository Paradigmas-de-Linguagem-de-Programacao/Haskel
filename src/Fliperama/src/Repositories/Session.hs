module Repositories.Session (
    saveSessionData,
    getLastMenuMessage,
    buildSessionData,
    getCurrentSessionPlayer,
    isThereALoggedPlayer
) where

import System.IO
import Data.List.Split (splitOn)
import DataTypes.Session (Session(..))

buildSessionData :: String -> String -> Session
buildSessionData owner menuMessage =
    Session { ownerUserName = owner, menuStateMessage = menuMessage }


saveSessionData :: Session -> IO()
saveSessionData session@(Session { ownerUserName=ownerUserName, menuStateMessage=menuStateMessage }) = do
    let path = "Repositories/data/session.txt"
    let newData = ownerUserName ++ ";" ++ menuStateMessage
    writeFile path newData

getCurrentSessionPlayer :: IO String
getCurrentSessionPlayer = do
    fileContent <- readFile "Repositories/data/session.txt"
    let str = parseSessionData fileContent
    if (length str == 0) then return ""
    else return $ str !! 0

parseSessionData :: String -> [String]
parseSessionData fileString = splitOn ";" fileString

isThereALoggedPlayer :: IO Bool
isThereALoggedPlayer = do
    fileContent <- readFile "Repositories/data/session.txt"
    let strArr = parseSessionData fileContent
    if ((length strArr) == 0 || (length (strArr !! 0)) == 0) then return False
    else return True

getLastMenuMessage :: IO String
getLastMenuMessage = do
    fileContent <- readFile "Repositories/data/session.txt"
    let str = parseSessionData fileContent
    if (length str < 2) then return ""
    else return $ str !! 1