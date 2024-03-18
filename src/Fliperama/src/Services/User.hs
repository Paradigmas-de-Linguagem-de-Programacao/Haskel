module Services.User (createNewUser, authUser) where

import Repositories.User as UserRepository
import DataTypes.User (User)

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

