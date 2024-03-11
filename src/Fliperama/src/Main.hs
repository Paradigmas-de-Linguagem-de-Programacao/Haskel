import System.IO
import Repositories.User (buildUser, writeUserData)
import Services.User (createNewUser, authUser)

main :: IO ()
main = do
    createNewUser "Yoyo" "Senha"
    return ()
