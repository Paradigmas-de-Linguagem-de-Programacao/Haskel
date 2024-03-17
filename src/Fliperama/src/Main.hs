import System.IO
import Services.User (authUser)
import Services.Actions (createNewUserAction)

main :: IO ()
main = do
    createNewUserAction
    main
