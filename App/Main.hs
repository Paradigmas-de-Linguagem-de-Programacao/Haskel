import Src.MyLib

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
mockTetrisMainFunction = playIO (InWindow "Tetris" resolucao posicaoinicial)
                                (light black)
                                60
                                geraEstadoInicial
                                renderizacao
                                inputTeclado
                                atualizaTempo
    

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
    