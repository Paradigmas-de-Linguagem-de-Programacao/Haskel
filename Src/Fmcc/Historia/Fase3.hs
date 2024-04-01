module Historia.Fase3(
    escolhaCaminhoCidadeFase3
) where
import Historia
import Loja
import SistemaGold
import Util.Lib
import CombateGPT

escolhaCaminhoCidadeFase3::IO()
escolhaCaminhoCidadeFase3 = do

    putStrLn  aposFinalDeSemana
    escolha03 <- trim <$> getLine
    clearScreen
    if escolha03 == "1" then do
        putStrLn "Você escolheu explorar a cidade em uma segunda, você é realmente um heroi, para onde nosso bravo web-guerreiro irá?\n"
        abreMapa03 ["(1) Voltar a loja do ferreiro Ferreira para *ver* os itens disponíveis.", "(2) Conversar com o Ferreira.", "(3) Ir à praça da cidade.","(4) Procrastinar um pouco.","(5) Voltar ao menu" ]
    else if escolha03 == "2" then do
        printString cursoHistoria3
        historiaPrincipal ["(1) Ganhar dinheiro","(2) Comprar poções com C.W.","(3) Visitar o ferreiro Ferreira","(4) Me garanto em enfrentar a I.A.","(5) Voltar ao menu"]
    else if escolha03 == "3" then
        voltaMenu
    else do
        putStrLn "Escolha uma opção válida."
        escolhaCaminhoCidadeFase3

abreMapa03 :: [String] -> IO()
abreMapa03 opcoes = do

    mapM_ putStrLn opcoes

    putStrLn "\n------------------------------------------------------------------------------------\n"

    resposta <- trim <$> getLine
    clearScreen
    if resposta == "1" then do
        verLoja
        abreMapa03 opcoes
    else if resposta == "2" then do
        printString dialogoFerreira03
        abreMapa03 opcoes
    else if resposta == "3" then do
        printString dialogoProfessorDublador
        abreMapa03 opcoes
    else if resposta == "4" then do
        printString cursoHistoria3
        historiaPrincipal ["(1) Ganhar dinheiro","(2) Comprar poções com C.W.","(3) Visitar o ferreiro Ferreira","(4) Me garanto em enfrentar a I.A.", "(5) Voltar ao menu"]
    else if resposta == "5" then
        voltaMenu
    else do
        putStrLn "Escreva uma opção válida."
        abreMapa03 opcoes

historiaPrincipal::[String]->IO()
historiaPrincipal opcoes = do
    putStrLn "O que deseja fazer agora que está de volta a cidade?\n"
    mapM_ putStrLn opcoes
    putStrLn "\n------------------------------------------------------------------------------------\n"

    opcaoJogador <- trim <$>getLine
    clearScreen
    if opcaoJogador == "1" then do
        putStrLn "Ganhar alguns mangos sempre é bom, talvez assim nosso herói não precise aderir a nenhuma greve. Como iremos angariar fundos?\n"
        sistemaGold
        historiaPrincipal opcoes
    else if opcaoJogador == "2" then do
        abreLojaPocoes
        historiaPrincipal opcoes
    else if opcaoJogador == "3" then do
        abreLojaItens
        historiaPrincipal opcoes
    else if opcaoJogador == "4" then
        vitoriaGPT
    else if opcaoJogador == "5" then
        voltaMenu
    else do 
        putStrLn "Digite uma opcão válida."
        historiaPrincipal opcoes
