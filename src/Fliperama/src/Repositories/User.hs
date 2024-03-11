module Repositories.User (buildUser, writeUserData, readUsersDataFile) where

import System.IO
import Data.List.Split (splitOn)
import Models.User (User(..))
import Control.Monad (liftM)

writeUserData :: User -> IO User
writeUserData user@(User name password) = do
    let path = "Repositories/data/users.txt"
    let newData = name ++ ";" ++ password ++ "\n"
    appendFile path newData
    return user

readUsersDataFile :: IO String
readUsersDataFile = readFile "Repositories/data/users.txt"

buildUser :: String -> String -> User
buildUser uname pass = User { username = uname, password = pass }
