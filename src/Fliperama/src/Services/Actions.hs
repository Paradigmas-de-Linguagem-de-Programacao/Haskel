module Services.Actions (
    createNewUserAction,
    loginUserAction,
    loginMenuAction,
    loggedUserMenuAction
) where

import Data.Char (toLower)
import qualified Services.User as UserServices
import qualified Services.Session as SessionServices
import qualified Repositories.Session as SessionRepository
import System.Console.ANSI


createNewUserAction :: IO()
createNewUserAction = do
    putStrLn "Escolha seu username: "
    usernameInput <- getLine
    putStrLn "Escolha uma senha: "
    passwordInput <- getLine

    UserServices.createNewUser usernameInput passwordInput   
    putStrLn "Usuário cadastrado com sucesso !!!\n"

loginUserAction :: IO()
loginUserAction = do 
    putStrLn "Digite seu username: "
    inputUsername <- getLine
    putStrLn "Digite sua senha: "
    inputPassword <- getLine

    isUserValid <- UserServices.authUser inputUsername inputPassword
    if (isUserValid) then SessionServices.setSessionData inputUsername ("Bem vindo " ++ inputUsername)
    else SessionServices.setSessionData "" "Não há usuários com as credenciais informadas"

drawInitialScreenAction :: IO()
drawInitialScreenAction = 
    putStrLn 
        " ____    _       ____                                            \n\
        \|  _ \\  | |     |  _ \\                                           \n\
        \| |_) | | |     | |_) |                                          \n\
        \|  __/  | |___  |  __/                                           \n\
        \|_|___  |_____| |_|                                              \n\
        \|  ___| | | (_)  _ __     ___   _ __    __ _   _ __ ___     __ _ \n\
        \| |_    | | | | | '_ \\   / _ \\ | '__|  / _` | | '_ ` _ \\   / _` |\n\
        \|  _|   | | | | | |_) | |  __/ | |    | (_| | | | | | | | | (_| |\n\
        \|_|     |_| |_| | .__/  \\___|  |_|     \\__,_| |_| |_| |_|  \\__,_|\n\
        \                |_|  \\/  |   ___   _ __    _   _                 \n\
        \                  | |\\/| |  / _ \\ | '_ \\  | | | |                \n\
        \                  | |  | | |  __/ | | | | | |_| |                \n\
        \                  |_|  |_|  \\___| |_| |_|  \\__,_|                "


drawLoggedUserScreenAction :: IO ()
drawLoggedUserScreenAction = putStrLn $
    " _____                        _   _                \n" ++
    "| ____|  ___    ___    ___   | | | |__     __ _    \n" ++
    "|  _|   / __|  / __|  / _ \\  | | | '_ \\   / _` |   \n" ++
    "| |___  \\__ \\ | (__  | (_) | | | | | | | | (_| |   \n" ++
    "|_____| |___/  \\___|  \\___/  |_| |_| |_|  \\__,_|   \n" ++
    " _   _   _ __ ___       (_)   ___     __ _    ___   \n" ++
    "| | | | | '_ ` _ \\      | |  / _ \\   / _` |  / _ \\ \n" ++
    "| |_| | | | | | | |     | | | (_) | | (_| | | (_) |\n" ++
    " \\__,_| |_| |_| |_|    _/ |  \\___/   \\__, |  \\___/ \n" ++
    "                      |__/           |___/          "



toLowerCase :: String -> String
toLowerCase str = map toLower str

logLastLoadedMessageAction :: IO()
logLastLoadedMessageAction = do
    lastMsg <- SessionRepository.getLastMenuMessage
    putStrLn $ lastMsg

loginMenuAction :: String -> IO String
loginMenuAction initialMessage = do
    clearScreenAction
    drawInitialScreenAction
    lastMenuMessage <- SessionRepository.getLastMenuMessage

    if ((length initialMessage) == 0) 
        then putStrLn lastMenuMessage
        else putStrLn initialMessage

    putStrLn "\nOpções: "
    putStrLn "R - Registrar-se"
    putStrLn "L - Fazer Login"
    putStrLn "\nDigite sua escolha: "
    selectedOption <- getLine
    clearScreenAction
    if((elem (toLowerCase selectedOption) ["r", "l", "t"]) == False)
        then loginMenuAction "Opção Inválida"
        else return (toLowerCase selectedOption)

loggedUserMenuAction :: String -> IO String
loggedUserMenuAction initialMessage = do
    clearScreenAction
    drawLoggedUserScreenAction
    lastMenuMessage <- SessionRepository.getLastMenuMessage

    if((length initialMessage) == 0)
        then putStrLn lastMenuMessage
        else putStrLn initialMessage

    putStrLn "\n Jogos Disponíveis: "
    putStrLn "T - Tetris"
    putStrLn "F - FMCC"
    putStrLn "\nDigite sua escolha: "
    selectedOption <- getLine
    clearScreenAction
    if((elem (toLowerCase selectedOption) ["t", "f"]) == False)
        then loggedUserMenuAction "Opção Inválida"
        else return (toLowerCase selectedOption)



clearScreenAction :: IO ()
clearScreenAction = clearScreen >> setCursorPosition 0 0
