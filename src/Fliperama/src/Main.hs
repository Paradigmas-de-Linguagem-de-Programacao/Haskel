import System.IO
import Repositories.User (buildUser, writeUserData)
import Services.User (createNewUser, authUser)

main :: IO ()
main = do
    createNewUser "Yoyo" "Senha"
    authResult <- authUser "Yoyo" "senha"
    putStrLn $ "Is valid user: " ++ show(authResult)
