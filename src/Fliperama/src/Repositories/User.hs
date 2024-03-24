module Repositories.User (buildUser, writeUserData, readUsersDataFile) where

import System.IO
import Data.List.Split (splitOn)
import DataTypes.User (User(..))

getUserDataFilePath :: String
getUserDataFilePath = "Repositories/data/users.txt"

writeUserData :: User -> IO User
writeUserData user@(User name password) = do
    let path = getUserDataFilePath
    let newData = name ++ ";" ++ password ++ "\n"
    appendFile path newData
    return user

readUsersDataFile :: IO String
readUsersDataFile = readFile getUserDataFilePath

buildUser :: String -> String -> User
buildUser uname pass = User { username = uname, password = pass }
