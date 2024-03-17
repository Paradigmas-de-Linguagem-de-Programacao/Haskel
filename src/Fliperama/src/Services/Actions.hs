module Services.Actions (createNewUserAction, loginUserAction, menuAction) where

import Data.Char (toLower)
import Services.User(createNewUser, authUser)
import System.Console.ANSI


createNewUserAction :: IO()
createNewUserAction = do
    putStrLn "Escolha seu username: "
    usernameInput <- getLine
    putStrLn "Escolha uma senha: "
    passwordInput <- getLine

    createNewUser usernameInput passwordInput   
    putStrLn "Usuário cadastrado com sucesso !!!\n"

loginUserAction :: IO()
loginUserAction = do 
    putStrLn "Digite seu username: "
    inputUsername <- getLine
    putStrLn "Digite sua senha: "
    inputPassword <- getLine

    isUserValid <- authUser inputUsername inputPassword
    if (isUserValid) then putStrLn "Autenticado"
    else putStrLn "Não existem usuários com as credenciais informadas"

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
        \|_|     |_| |_| | .__/  \\___| |_|     \\__,_| |_| |_| |_|  \\__,_|\n\
        \                |_|  \\/  |   ___   _ __    _   _                 \n\
        \                  | |\\/| |  / _ \\ | '_ \\  | | | |                \n\
        \                  | |  | | |  __/ | | | | | |_| |                \n\
        \                  |_|  |_|  \\___| |_| |_|  \\__,_|                "

toLowerCase :: String -> String
toLowerCase str = map toLower str

menuAction :: String -> IO String
menuAction initialMessage = do
    clearScreenAction
    drawInitialScreenAction
    putStrLn initialMessage 
    putStrLn "\nOpções: "
    putStrLn "R - Registrar-se"
    putStrLn "L - Fazer Login"
    putStrLn "\nDigite sua escolha: "
    selectedOption <- getLine
    clearScreenAction
    if((elem (toLowerCase selectedOption) ["r", "l"]) == False)
        then menuAction "Opção Inválida"
        else return (toLowerCase selectedOption)

clearScreenAction :: IO ()
clearScreenAction = clearScreen >> setCursorPosition 0 0
