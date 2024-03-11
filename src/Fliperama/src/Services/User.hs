module Services.User (createNewUser) where

import Repositories.User (buildUser, loadUsers, writeUserData)
import Models.User (User)

createNewUser :: String -> String -> IO User
createNewUser username password = 
    writeUserData user
    where
        user = buildUser username password
        
