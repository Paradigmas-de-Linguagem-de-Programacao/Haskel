module Historia.Final(
    fimDeJogo
) where
import Historia
import Util.Lib

fimDeJogo::IO()
fimDeJogo = do

    putStrLn  final
    escolha03 <- trim <$> getLine
    abreMapa04 ["(1) Conversar com o Ferreira.", "(2) Ir à praça da cidade.","(3) Ir à um beco escuro que está te chamando" , "(4) Ir à mestre dos magos.","(5) Voltar ao menu"]

abreMapa04 :: [String] -> IO()
abreMapa04 opcoes = do

    mapM_ putStrLn opcoes

    putStrLn "\n------------------------------------------------------------------------------------\n"

    resposta <- trim <$> getLine
    clearScreen
    if resposta == "1" then do
        putStrLn dialogoFerreira04
        abreMapa04 opcoes
    else if resposta == "2" then do
        putStrLn dialogoTodos
        abreMapa04 opcoes
    else if resposta == "3" then do
        putStrLn dialogoPadeiro02
        abreMapa04 opcoes
    else if resposta == "4" then do
        putStrLn finalHistoria
        esperandoEnter
    else if resposta == "5" then
        voltaMenu
    else do
        putStrLn "Escreva uma opção válida."
        abreMapa04 opcoes