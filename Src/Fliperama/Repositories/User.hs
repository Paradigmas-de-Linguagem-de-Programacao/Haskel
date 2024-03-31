module Repositories.User (
    buildUser, 
    writeUserData, 
    readUsersDataFile, 
    rewriteUserData, 
    getUserDataFilePath
    ) where

import System.IO

import Data.List.Split (splitOn)

import DataTypes.User (
    User(..)
    )

import Control.Exception (evaluate)

getUserDataFilePath :: String
getUserDataFilePath = "Repositories/data/users.txt"

writeUserData :: User -> IO User
writeUserData user@(User name password) = do
    let path = getUserDataFilePath
    let newData = name ++ ";" ++ password ++ "\n"
    appendFile path newData
    return user

rewriteUserData :: String -> IO()
rewriteUserData newData = do
    let path = getUserDataFilePath
    withFile path WriteMode $ \handle -> do
        hPutStrLn handle newData

readUsersDataFile :: IO String
readUsersDataFile = withFile getUserDataFilePath ReadMode $ \handle -> do
    contents <- hGetContents handle
    evaluate (length contents)
    return contents

buildUser :: String -> String -> User
buildUser uname pass = User { username = uname, password = pass }
