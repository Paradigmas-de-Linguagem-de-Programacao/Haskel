import MyLib (
    loginUserAction, createNewUserAction, deleteUserAction,
    resetSessionState, startTetris, startFmcc,
    saveSessionData, getLastMenuMessage, buildSessionData,
    getCurrentSessionPlayer, isThereALoggedPlayer, deleteSessionData,
    loggedUserMenuAction, loginMenuAction)

import System.IO

userOptionManager :: String -> IO()
userOptionManager userOpt 
    | userOpt == "l" = loginUserAction
    | userOpt == "r" = createNewUserAction
    | userOpt == "t" = mockTetrisMainFunction
    | userOpt == "f" = mockFMCCMainFunction
    | userOpt == "s" = resetSessionState
    | userOpt == "d" = deleteUserAction

mockTetrisMainFunction :: IO()
mockTetrisMainFunction = startTetris
    

mockFMCCMainFunction :: IO()
mockFMCCMainFunction = startFmcc

main :: IO ()
main = do
    playerLogged <- isThereALoggedPlayer
    userOption <- if playerLogged
                  then loggedUserMenuAction ""
                  else loginMenuAction ""
    userOptionManager userOption
    main
    