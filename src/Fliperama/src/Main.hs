import System.IO
import Services.User (authUser)
import Services.Actions (createNewUserAction, menuAction)

main :: IO ()
main = do
    userOption <- menuAction ""
    main
