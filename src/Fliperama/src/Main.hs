import System.IO
import Services.Actions (createNewUserAction, menuAction, loginUserAction)
import qualified Services.Session as SessionServices

userOptionManager :: String -> IO()
userOptionManager userOpt 
    | userOpt == "l" = loginUserAction
    | userOpt == "r" = createNewUserAction

{- Se preciso for, salvar estado entre loops de main através do file system -}
main :: IO ()
main = do
    userOption <- menuAction ""
    userOptionManager userOption
    main
