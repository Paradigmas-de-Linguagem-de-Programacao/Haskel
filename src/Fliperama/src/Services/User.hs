module Services.User (createNewUser, authUser) where

import Repositories.User (buildUser, writeUserData, readUsersDataFile)
import Models.User (User)

checkCredentials :: String -> String -> [String] -> Bool
checkCredentials _ _ [] = False
checkCredentials username password (dataHead : dataTail) 
    | dataHead == username ++ ";" ++ password = True
    | otherwise = checkCredentials username password dataTail


createNewUser :: String -> String -> IO User
createNewUser username password = 
    writeUserData user
    where
        user = buildUser username password


authUser :: String -> String -> IO Bool
authUser username password = do
    usersData <- readUsersDataFile
    let usersArr = lines usersData
    return $ checkCredentials username password usersArr

