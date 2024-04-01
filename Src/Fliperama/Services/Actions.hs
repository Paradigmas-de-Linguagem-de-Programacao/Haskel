module Fliperama.Services.Actions (
    createNewUserAction,
    loginUserAction,
    loginMenuAction,
    loggedUserMenuAction,
    resetSessionState,
    deleteUserAction
) where

import Data.Char (toLower)
import qualified Fliperama.Services.User as UserServices
import qualified Fliperama.Services.Session as SessionServices
import qualified Fliperama.Repositories.Session as SessionRepository
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.Directory
import System.IO


createNewUserAction :: IO()
createNewUserAction = do
    putStrLn "Escolha seu username: "
    usernameInput <- getLine
    putStrLn "Escolha uma senha: "
    passwordInput <- getLine
    criaDiretorio usernameInput

    UserServices.createNewUser usernameInput passwordInput   
    SessionServices.setSessionData "" "Usuário cadastrado com sucesso"

criaDiretorio :: String-> IO()
criaDiretorio nome = do
    let caminho = "./Src/Fliperama/Repositories/data/Jogadores/" ++ nome
    createDirectory $ caminho
    writeFile (caminho ++ "/conquistaFmcc.txt") $ unlines (conquistas)
    writeFile (caminho ++ "/conquistaTetris.txt") "sla2.0"
    writeFile (caminho ++ "/progressoFmcc.txt") "0"

conquistas :: [String]
conquistas = ["Conquista {nomeC = \"Jubilado\", descricao = \"Negue Carl Wilson 8 vezes\", alcancou = False}", "Conquista {nomeC = \"Se voce nao parar eu Paro\", descricao = \"Tome 6 Pocoes\", alcancou = False}", "Conquista {nomeC = \"Faixa Preta\", descricao = \"Derrotou ConversaGPT\", alcancou = False}"]

removeDiretorio :: String -> IO()
removeDiretorio nome = do
    let caminho = "./Src/Fliperama/Repositories/data/Jogadores/" ++ nome
    removeDirectoryRecursive $ caminho

deleteUserAction :: IO()
deleteUserAction = do
    putStrLn "Digite seu username: "
    inputUsername <- getLine
    putStrLn "Digite sua senha: "
    inputPassword <- getLine

    isUserValid <- UserServices.authUser inputUsername inputPassword
    if (isUserValid) 
        then do
            UserServices.deleteUser inputUsername inputPassword
            SessionServices.setSessionData "" ("Usuário " ++ inputUsername ++ " deletado com sucesso")
            removeDiretorio inputUsername
    else SessionServices.setSessionData "" "Não há usuários com as credenciais informadas"


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
    putStrLn "D - Deletar Conta"
    putStrLn "\nDigite sua escolha: "
    selectedOption <- getLine
    clearScreenAction
    if((elem (toLowerCase selectedOption) ["r", "l", "t", "d"]) == False)
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
    putStrLn "S - Sair"
    putStrLn "\nDigite sua escolha: "
    selectedOption <- getLine
    clearScreenAction
    if((elem (toLowerCase selectedOption) ["t", "f", "s"]) == False)
        then loggedUserMenuAction "Opção Inválida"
        else return (toLowerCase selectedOption)


clearScreenAction :: IO ()
clearScreenAction = clearScreen >> setCursorPosition 0 0

resetSessionState :: IO()
resetSessionState = SessionRepository.deleteSessionData