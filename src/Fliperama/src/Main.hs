import System.IO
import Repositories.User (loadUsers, buildUser, writeUserData)
import Services.User (createNewUser)

main :: IO ()
main = do
    createNewUser "Yohan" "ZECAPadoguinho"
    loadUsers
