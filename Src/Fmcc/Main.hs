module Fmcc.Main(
    startFmcc
) where
import Fmcc.Util.Lib
import System.Directory (createDirectory, doesDirectoryExist)
import Fmcc.Util.ControleSalvamento

startFmcc::IO()
startFmcc = start

start::IO()
start = do
    mapeiaProgresso
    clearScreen
    putStrLn slogan
    esperandoEnter
    menu


menu :: IO ()
menu = do
    clearScreen
    putStrLn "Bem-vindo a Fábulas de Magia : Cidadela de Cristal, mais conhecido como FMCC "
    putStrLn (textoFormatado "")
    putStrLn "Escolha uma opção:"
    putStrLn "1 - Começar o jogo"
    putStrLn "2 - Carregar o jogo"
    putStrLn "3 - Ajuda"
    putStrLn "4 - Sair\n"

    escolha <- trim <$> getLine 
    clearScreen
    case escolha of
        "1" -> comecaJogo >> menu
        "2" -> carregaJogo >> menu
        "3" -> help >> esperandoEnter >> clearScreen >> menu
        "4" -> putStrLn fechaJogo
        _   -> putStrLn "Opção inválida. Por favor, escolha uma opção válida.\n" >> esperandoEnter >> clearScreen >> menu

