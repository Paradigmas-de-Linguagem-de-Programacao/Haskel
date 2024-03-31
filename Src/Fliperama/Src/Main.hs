import Src.Fliperama.Src.Tetris.Main (
    renderizacao,
    inputTeclado,
    atualizaTempo
    )

import System.IO

import Services.Actions (
    createNewUserAction,
    loginMenuAction,
    loggedUserMenuAction,
    loginUserAction,
    resetSessionState,
    deleteUserAction)

import Repositories.Session (isThereALoggedPlayer)

import qualified Services.Session as SessionServices

import Src.Fliperama.Src.Tetris.Util.Estado (geraEstadoInicial)



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
    