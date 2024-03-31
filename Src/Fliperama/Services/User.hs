module Fliperama.Services.User (createNewUser, authUser, deleteUser) where

import System.IO
import Fliperama.Repositories.User as UserRepository
import Fliperama.DataTypes.User (User)
import Data.List.Split (splitOn)
import Control.Exception (evaluate)

checkCredentials :: String -> String -> [String] -> Bool
checkCredentials _ _ [] = False
checkCredentials username password (dataHead : dataTail) 
    | dataHead == username ++ ";" ++ password = True
    | otherwise = checkCredentials username password dataTail

createNewUser :: String -> String -> IO User
createNewUser username password = 
    UserRepository.writeUserData user
    where
        user = UserRepository.buildUser username password

authUser :: String -> String -> IO Bool
authUser username password = do
    usersData <- UserRepository.readUsersDataFile
    let usersArr = lines usersData
    return $ checkCredentials username password usersArr

concatenateArrayIntoString :: [String] -> String
concatenateArrayIntoString (x : xs)
    | x == "" && xs == [] = ""
    | x == "" && xs /= [] = concatenateArrayIntoString xs
    | x /= "" && xs == [] = x ++ "\n"
    | otherwise = x ++ "\n" ++ concatenateArrayIntoString xs

filterUsersData :: String -> String -> IO String
filterUsersData username password = 
    withFile getUserDataFilePath ReadMode $ \handle -> do
        usersData <- hGetContents handle
        evaluate (length usersData)
        let splitedByLine = lines usersData
        let filteredData = filter (\x -> (x /= username++";"++password)) splitedByLine
        let newData = concatenateArrayIntoString filteredData
        return newData

deleteUser :: String -> String -> IO()
deleteUser username password = do
    filteredUsersDataString <- filterUsersData username password
    UserRepository.rewriteUserData filteredUsersDataString