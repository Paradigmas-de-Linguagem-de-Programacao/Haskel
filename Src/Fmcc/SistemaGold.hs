module SistemaGold where
import System.IO
import Control.Concurrent(threadDelay)
import Historia
import Models.Player
import Util.Lib

sistemaGold :: IO ()
sistemaGold = do

    putStrLn "(1) Trabalhar ajudando a carregar alguns suprimentos para um mercado mágico próximo.\n(2) Responder enigmas que as IAs utilizam para afirmar sua superioridade.\n(3) N.D.A."
    putStrLn "\n------------------------------------------------------------------------------------\n"

    input <- getLine
    clearScreen
    if trim input == "1" then do
        putStrLn explicacaoBasica01
        putStrLn "*QUANDO QUISER PARAR, APERTE QUALQUER TECLA.*"
        sistemaGoldPassivoAux 0
    else if trim input == "2"
        then sistemaGoldAtivoAux
    else do
        putStrLn (textoFormatado("Não quer trabalhar hein, tudo bem. Boa sorte!\n"))
        esperandoEnter

sistemaGoldPassivoAux::Int->IO()
sistemaGoldPassivoAux goldAntigo= do
    putStrLn "carregando caixas..."
    input <- hReady stdin
    if input then do
        _ <- getLine
        heanes <- carregaPlayer
        let heanes2 = modificaGold heanes (goldAntigo + 3)
        salvaPlayer heanes2
        clearScreen
        putStrLn (textoFormatado("\nC.W.:Você se cansa rápido para um héroi...\n\n*Você olha para sua carteira de professor, e vê " ++ show (getGold heanes) ++ " moedas de critais.*\n"))
        esperandoEnter
        clearScreen
    else do
        threadDelay (3*1000000)
        sistemaGoldPassivoAux (goldAntigo+3)

sistemaGoldAtivoAux:: IO()
sistemaGoldAtivoAux = do
    clearScreen
    heanes <- carregaPlayer
    putStrLn promptPerguntas1
    esperandoEnter
    clearScreen
    putStrLn promptPerguntas2
    esperandoEnter
    clearScreen
    putStrLn promptPerguntas3
    esperandoEnter
    clearScreen

    clearScreen
    let progresso = getProgresso heanes
    case progresso of
        1 -> questao01 heanes
        2 -> questao02 heanes
        3 -> ultimasPerguntas heanes
        _ -> return ()
    esperandoEnter

questao01:: Player ->IO()
questao01 heanes = do
    putStrLn (textoFormatado "Vending Machine: QUAL A MELHOR LINGUAGEM DE PROGRAMAÇÃO JÁ CRIADA?\n")
    input <- getLine
    clearScreen
    if (uppercase input /= "HASKELL") then do
        let heanesMaisRico = modificaGold heanes 50
        salvaPlayer heanesMaisRico
        putStrLn "Vending Machine: MUITO BEM, HUMANO! AINDA BEM QUE MEU PROGRAMADOR NÃO ME FEZ EM HASKELL... ECA!"
        putStrLn (parabenizaAcerto heanesMaisRico)
        putStrLn "------------------------------------------------------------------------------------\n"
    else do
        putStrLn maquinaIrritada 
        putStrLn "------------------------------------------------------------------------------------\n"

questao02::Player -> IO()
questao02 heanes = do
    putStrLn (textoFormatado "Vending Machine: A QUANTOS ANOS A CIDADELA DE CRISTAIS FOI FUNDADA?\n")
    input <- getLine
    clearScreen
    if trim input == "500" then do
        let heanesMaisRico = modificaGold heanes 50
        salvaPlayer heanesMaisRico
        putStrLn "\nVending Machine: MUITO BEM, HUMANO! SÃO 500 ANOS DE HISTÓRIA!"
        putStrLn (parabenizaAcerto heanesMaisRico)
        putStrLn "------------------------------------------------------------------------------------\n"
    else do
        putStrLn maquinaIrritada
        putStrLn "------------------------------------------------------------------------------------\n"

ultimasPerguntas ::Player -> IO ()
ultimasPerguntas heanes = do
    clearScreen
    putStrLn propagandaMaquina
    putStrLn "Ai vai a primeira pergunta:"
    questao03 heanes
    putStrLn "Bom não se deixe influenciar pelo seu resultado passado, vamos para a última e derradeira pergunta teras o que é necessario para responder ela?!"
    questao04 heanes
    
questao03:: Player -> IO()
questao03 heanes = do
    putStrLn (textoFormatado "Vending Machine: QUAL O MAIOR TIME DE FUTEBOL DA PARAÍBA?\n")
    input <- getLine
    clearScreen
    if comparaString input "Galo" || comparaString input "Treze" then do
        let heanesMaisRico = modificaGold heanes 50
        salvaPlayer heanesMaisRico
        putStrLn  "\nVending Machine: MUITO BEM, HUMANO! UH É GALO DOIDO!"
        putStrLn (parabenizaAcerto heanesMaisRico)
        putStrLn "------------------------------------------------------------------------------------\n"
    else do 
        putStrLn maquinaIrritada
        putStrLn "------------------------------------------------------------------------------------\n"

questao04:: Player -> IO()
questao04 heanes = do
    putStrLn (textoFormatado "Vending Machine: QUAL A SEGUNDA LINGUAGEM DE PROGRAMAÇÃO QUE SERÁ UTILIZADA NA CADEIRA DE PLP?\n")
    input <- getLine
    clearScreen
    if comparaString input "Prolog" || comparaString input "prolog" then do
        let heanesMaisRico = modificaGold heanes 50
        salvaPlayer heanesMaisRico
        
        putStrLn "\nVending Machine: MUITO BEM, HUMANO! O PARADIGMA LÓGICO VAI SER EXPLORADO NA DISCIPLINA!"
        putStrLn (parabenizaAcerto heanesMaisRico)
        putStrLn "------------------------------------------------------------------------------------\n"
    else do
        putStrLn maquinaIrritada
        putStrLn "------------------------------------------------------------------------------------\n"


parabenizaAcerto:: Player -> String
parabenizaAcerto heanes = "\n*Você olha para sua carteira de professor, e vê " ++ show (getGold heanes) ++ " moedas de critais\n"

maquinaIrritada :: String
maquinaIrritada ="Vending Machine: COMO VOCÊ PODE FALAR UMA COISA DESSAS??? REPENSE SUAS ATITUDEZINHAS, SEM REWARD PARA VOCÊ.\n\n*Você olha para sua carteira de professor, e ela está vazia, você pensa em entrar em greve. Enquanto isso, você se vê de volta a cidade meio cabisbaixo*\n"