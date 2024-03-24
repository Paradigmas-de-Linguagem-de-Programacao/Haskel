import System.IO
import Services.Actions (
    createNewUserAction,
    loginMenuAction,
    loggedUserMenuAction,
    loginUserAction)
import Repositories.Session (isThereALoggedPlayer)
import qualified Services.Session as SessionServices

userOptionManager :: String -> IO()
userOptionManager userOpt 
    | userOpt == "l" = loginUserAction
    | userOpt == "r" = createNewUserAction

main :: IO ()
main = do
    playerLogged <- isThereALoggedPlayer
    userOption <- if playerLogged
                  then loggedUserMenuAction ""
                  else loginMenuAction ""
    userOptionManager userOption
    main
