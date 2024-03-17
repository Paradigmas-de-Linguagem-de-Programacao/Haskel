import System.IO
import Services.User (authUser)
import Services.Actions (createNewUserAction, menuAction, loginUserAction)

userOptionManager :: String -> IO()
userOptionManager userOpt 
    | userOpt == "l" = loginUserAction
    | userOpt == "r" = createNewUserAction

main :: IO ()
main = do
    userOption <- menuAction ""
    userOptionManager userOption
    main
