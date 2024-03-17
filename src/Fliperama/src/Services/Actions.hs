module Services.Actions (createNewUserAction) where

import Services.User(createNewUser)


createNewUserAction :: IO()
createNewUserAction = do
    putStrLn "Escolha seu username: "
    usernameInput <- getLine
    putStrLn "Escolha uma senha: "
    passwordInput <- getLine

    createNewUser usernameInput passwordInput
    putStrLn "Usuário cadastrado com sucesso !!!"