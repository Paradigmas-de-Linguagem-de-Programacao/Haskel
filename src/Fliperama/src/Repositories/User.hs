module Repositories.User (loadUsers, buildUser, writeUserData) where

import System.IO
import Data.List.Split (splitOn)
import Models.User (User(..))
import Control.Monad (liftM)

readUserDataFile :: FilePath -> IO String
readUserDataFile path = 
    content
    where
        content = readFile path

readUserDataFileAsString :: FilePath -> IO String
readUserDataFileAsString path = fmap id (readUserDataFile path)

buildUser :: String -> String -> User
buildUser uname pass = User { username = uname, password = pass }

writeUserData :: User -> IO User
writeUserData user@(User name password) = do
    let path = "Repositories/data/users.txt"
    let userData = name ++ ";" ++  password
    writeFile path userData
    return user


loadUsers :: [String]
loadUsers =
    let contents = readUserDataFileAsString "Repositories/data/users.txt" -- contents :: String
    in splitOn "\n" <$> contents  -- splitOn "\n" <$> contents :: [String]
