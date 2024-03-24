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
    | userOpt == "t" = mockTetrisMainFunction
    | userOpt == "f" = mockFMCCMainFunction
    | userOpt == "s" = mockResetAppState

mockResetAppState :: IO()
mockResetAppState = putStrLn "Reseted app state"

mockTetrisMainFunction :: IO()
mockTetrisMainFunction = do
    putStrLn "Tetris RUNNED"

mockFMCCMainFunction :: IO()
mockFMCCMainFunction = do
    putStrLn "FMCC RUNNED"

main :: IO ()
main = do
    playerLogged <- isThereALoggedPlayer
    userOption <- if playerLogged
                  then loggedUserMenuAction ""
                  else loginMenuAction ""
    userOptionManager userOption
    main
