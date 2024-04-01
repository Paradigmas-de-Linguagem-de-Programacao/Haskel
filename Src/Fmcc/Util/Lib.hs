module Util.Lib where

import System.Process
import Models.Conquista
import Models.Item
import Models.Player
import Models.Pocao
import System.IO
import Models.Inimigo
import Data.Char

textoFormatado::String->String
textoFormatado texto = texto ++ "\n------------------------------------------------------------------------------------\n"

removeOpcao::String->[String]->[String]
removeOpcao _ [] = []
removeOpcao opcao (a:as)
    | a == opcao = as
    | otherwise = a : removeOpcao opcao as

clearScreen::IO()
clearScreen = do
    _ <- system "clear"
    putStr "\ESC[H"

carregaPlayer :: IO Player
carregaPlayer = do
        handle <- openFile playerCaminho ReadMode
        conteudo <- hGetContents' handle
        hClose handle
        return (read conteudo :: Player)

carregaInimigo :: String -> IO Inimigo
carregaInimigo filepath = do
        handle <- openFile filepath ReadMode
        conteudo <- hGetContents' handle
        hClose handle
        return (read conteudo :: Inimigo)

salvaPlayer:: Player -> IO()
salvaPlayer heanes = writeFile playerCaminho (show heanes)

salvaItens:: [Item] -> IO()
salvaItens itens= do
    let listaItens = unlines (map show itens)
    writeFile itemCaminho listaItens

salvaPocao:: [Pocao] -> IO()
salvaPocao pocoes =do
     let listaPocao= unlines (map show pocoes)
     writeFile pocaoCaminho listaPocao

salvaConquista :: [Conquista] -> IO ()
salvaConquista conquistas = writeFile conquistaCaminho $ unlines (map show conquistas)

desbloqueaConquista :: String -> IO()
desbloqueaConquista nomeConquista = do
    conquistas <- carregaConquista
    let conquistaDesbloqueda = desbloquearConquista nomeConquista conquistas
    salvaConquista conquistaDesbloqueda

carregaConquista :: IO [Conquista]
carregaConquista = do
        handle <- openFile conquistaCaminho ReadMode
        conteudo <- hGetContents' handle
        hClose handle
        return (map parseConquista (lines conteudo))


desbloquearConquista :: String -> [Conquista] -> [Conquista]
desbloquearConquista nomeConquista =
     map (\c -> if nomeC c == nomeConquista then c { alcancou = True } else c)

parseConquista :: String -> Conquista
parseConquista texto = read texto :: Conquista

fechaJogo :: String
fechaJogo = "Então você decidiu dar uma pausa. Tudo bem... Espero que você volte. Aqui não é o mesmo sem você."

esperandoEnter :: IO ()
esperandoEnter = do
    putStr "Pressione Enter para continuar...\n"
    hFlush stdout
    _ <- getChar
    return ()

criaCaminho :: String -> String
criaCaminho nomeInimigo = "./src/pacote/" ++ nomeInimigo ++".txt"

atualizaProgresso::Int -> IO()
atualizaProgresso novoProgresso = do
    heanes <- carregaPlayer
    let novoHeanes = attProgresso heanes novoProgresso
    salvaPlayer novoHeanes

comparaString :: String -> String -> Bool
comparaString str1 str2 = clean str1 == clean str2
    where clean = map toLower . trim

{-esse droWhile ficando fazendo o drop da lista enquanto o boll que passamos é verdadeiro só que nesse caso
o bool que passamos lambda que se o primeiro caracter for espaço vazio ou um tab ele vai dar drop -}
apagaEspacos :: String -> String
apagaEspacos = dropWhile (\c -> c == ' ' || c == '\t')

{-esse metodo recebe uma string e usa composição de função pra resolver ela, tipo em matematica quando faz
f(x) g(x)  e g(f(x)) so que no caso a composição é (reverse (apagaEspacos (reverse (palavra))))-}
apagaEspacosFim :: String -> String
apagaEspacosFim = reverse . apagaEspacos . reverse

trim :: String -> String
trim = apagaEspacos . apagaEspacosFim

uppercase::String->String
uppercase "" = ""
uppercase (a:as) = [(toUpper a)] ++ uppercase as

identificaItem::String->[Item]->Maybe Item
identificaItem _ [] = Nothing
identificaItem nomeItem (a:as)
    | comparaString (getNomeItem a) nomeItem = Just a
    | otherwise = identificaItem nomeItem as

pegaItem::String->[Item]->Item
pegaItem _ [] = error "Item errado" --nunca chega nesse codigo
pegaItem nomeItem (a:as)
    | comparaString (getNomeItem a) nomeItem = a
    | otherwise = pegaItem nomeItem as

removeItem::String->[Item]->[Item]
removeItem _ [] = []
removeItem nomeItem (a:as)
    | comparaString (getNomeItem a) nomeItem = as
    | otherwise = a : removeItem nomeItem as

identificaPocao::String->[Pocao]->Maybe Pocao
identificaPocao _ [] = Nothing
identificaPocao nomePocao (a:as)
    | comparaString (getNomePocao a) nomePocao = Just a
    | otherwise = identificaPocao nomePocao as

identificaPocaoJaComprada::String->[Pocao]->Bool
identificaPocaoJaComprada _ [] = False
identificaPocaoJaComprada nomePocao (a:as)
    | comparaString (getNomePocao a) nomePocao = True
    | otherwise = identificaPocaoJaComprada nomePocao as

pegaPocao::String->[Pocao]->Pocao
pegaPocao _ [] = error "Poção errada" --nunca chega nesse codigo
pegaPocao nomePocao (a:as)
    | comparaString (getNomePocao a) nomePocao = a
    | otherwise = pegaPocao nomePocao as

removePocaoAntiga::String->[Pocao]->[Pocao]
removePocaoAntiga _ [] = []
removePocaoAntiga nomePocao (a:as)
    | comparaString (getNomePocao a) nomePocao = as
    | otherwise = a : removePocaoAntiga nomePocao as

resetPlayer:: IO Player
resetPlayer = do
    heanes <- carregaPlayer
    let heanesReset = reset heanes
    salvaPlayer heanesReset
    return heanesReset

resetPlayerGold::IO()
resetPlayerGold = do
    heanes <- carregaPlayer
    heanesResetado<- resetPlayer
    let heanesGold = modificaGold heanesResetado (getGold heanes)
    salvaPlayer heanesGold

salvaInimigo:: Inimigo -> FilePath -> IO()
salvaInimigo inimigo caminho = writeFile caminho (show inimigo)

progressoHeroi:: IO Int
progressoHeroi = getProgresso <$> resetPlayer

voltaMenu:: IO()
voltaMenu = do
    clearScreen
    putStrLn (textoFormatado "Então nosso heroi precisa voltar ao menu para pensar sobre a vida não é? Tudo bem mas por favor volte o mundo precisa de você\n")
    esperandoEnter

pocaoCaminho :: String
pocaoCaminho = "./src/pacote/Pocao.txt"

itemCaminho :: String
itemCaminho = "./src/pacote/Itens.txt"

conquistaCaminho :: String
conquistaCaminho = "./src/pacote/Conquista.txt"

playerCaminho :: String
playerCaminho = "./src/pacote/Heroi.txt"

printString:: String -> IO()
printString texto = do
    clearScreen
    putStrLn texto
    esperandoEnter

heanesString :: String
heanesString = "Player {nome = \"Heanes\", vida = 100, gold = 1000, defesa = 5, ataque = 5, equipamentos = [], pocoes = [], progresso = 0, pocoesTomadas = 0}"

armaduraCouro :: String
armaduraCouro =  "Item {nome = \"Armadura de couro\", preco = 30, ataque = 0, defesa = 30, descricao = \"Armadura que protege contra as falacias da IA.\"}"

armaduraFerro :: String
armaduraFerro = "Item {nome = \"Armadura de ferro\", preco = 100, ataque = 0, defesa = 60, descricao = \"Composta pelos mais refinados pedacos de sucata metalica, e garantido que esta armadura fara voce parecer o homem de ferro da shopee\"}"

espadaPedra::  String
espadaPedra = "Item {nome = \"Espada de pedra\", preco = 30, ataque = 30, defesa = 0, descricao = \"Se quiser espada tem, mas so usa espada quem nao se garante.\"}"

espadaFerro:: String
espadaFerro = "Item {nome = \"Espada de ferro\", preco = 100, ataque = 60, defesa = 0, descricao = \"Espada um pouco enferrujada, se nao matar na espadada mata no tetano.\"}"

espadaDiamante :: String
espadaDiamante = "Item {nome = \"Espada de diamante\", preco = 160, ataque = 100, defesa = 0, descricao = \"Espada do minecraft.\"}"

armaduraDiamante :: String
armaduraDiamante = "Item {nome = \"Armadura de diamante\", preco = 160, ataque = 0, defesa = 100, descricao = \"Armadura do minecraft.\"}"

cafe:: String
cafe = "Pocao {nome = \"Cafe\", vida = 40, preco = 20, defesa = 0, ataque = 0, quantidade = 1}"

redBull:: String
redBull = "Pocao {nome = \"Red Bull\", vida = 30, preco = 60, defesa = 20, ataque = 20, quantidade =1}"

caramelo:: String
caramelo = "Inimigo {nome = \"Cachorros Caramelos\", ataque = 40, defesa = 5, vida = 50, habilidadeEspecial = 0}"

kanva:: String
kanva = "Inimigo {nome = \"Kanva\", ataque = 55, defesa = 10, vida = 200, habilidadeEspecial = 75}"

playHub :: String
playHub = "Inimigo {nome = \"PlayHub\", ataque = 40, defesa = 0, vida = 300, habilidadeEspecial = 85}"

conversaGPT :: String
conversaGPT = "Inimigo {nome = \"ConversaGPT\", ataque = 120, defesa = 30, vida = 5000, habilidadeEspecial = 160}"

inimigo:: String -> Inimigo
inimigo = read

item::String -> Item
item = read

player ::Player
player = read heanesString

pocao :: String -> Pocao
pocao = read

comecaFase2::IO()
comecaFase2 = do
    resetPlayerGold
    atualizaProgresso 2
    let hub = inimigo playHub
    salvaInimigo hub (criaCaminho (getNomeInimigo hub))
    salvaItens [item espadaFerro ,item armaduraFerro]
    let pocaoItem = [pocao cafe, pocao redBull]
    salvaPocao pocaoItem

comecaFase3::IO()
comecaFase3 = do
    resetPlayerGold
    atualizaProgresso 3
    let gpt = inimigo conversaGPT
    salvaInimigo gpt (criaCaminho (getNomeInimigo gpt))
    salvaItens [item espadaDiamante, item armaduraDiamante]



slogan:: String
slogan =  "----------------------------------------------------------------------------------\n"++
          " ███████████       ██           ███         ██████████         ██████████       | \n" ++
          " ███               ████        ████        ███                ███               | \n" ++
          " ███               ██ ██      ██ ██        ███                ███               |\n" ++
          " ██████████        ██  ██    ██  ██        ███                ███               |\n" ++
          " ███               ██    ██ ██   ██        ███                ███               |\n" ++
          " ███               ██     ███    ██         ██████████         ██████████       |\n" ++
          "----------------------------------------------------------------------------------"



