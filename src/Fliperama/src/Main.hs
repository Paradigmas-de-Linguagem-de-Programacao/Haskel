import System.IO
import Services.Actions (createNewUserAction, loginMenuAction, loginUserAction)
import qualified Services.Session as SessionServices

userOptionManager :: String -> IO()
userOptionManager userOpt 
    | userOpt == "l" = loginUserAction
    | userOpt == "r" = createNewUserAction

main :: IO ()
main = do

    userOption <- loginMenuAction ""
    userOptionManager userOption
    main
