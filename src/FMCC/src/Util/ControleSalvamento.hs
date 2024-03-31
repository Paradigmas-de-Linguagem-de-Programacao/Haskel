{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Util.ControleSalvamento(
    comecaJogo , help, carregaJogo
) where
import Util.Lib
import Models.Item
import Models.Player
import Models.Conquista
import Models.Pocao
import Models.Inimigo
import Historia.Fase1
import Historia.Fase2
import Historia.Fase3
import Historia.Final
import Historia.Prologo
import Data.List ( delete )

comecaJogo::IO()
comecaJogo = do
    putStrLn "Inicializando dados"
    inicializaDados
    comecoHistoria


help::IO()
help = putStrLn menuAjuda
    where
        menuAjuda :: String
        menuAjuda = unlines
            [ "Olá amigo viajante! Estou aqui para esclarecer algumas dúvidas que talvez você possa ter:\n"
            , "Em primeiro plano, este jogo é um RPG textual que segue o estilo IDLE para ganhar recursos. Além disso, ele é munido de 3 fases."
            , "Se você criou um jogo e após entrar na sua conta novamente você optou por inicializar o game,"            , "você perderá todo seu progresso, ou seja, TOME BASTANTE CUIDADO."
            , "Além disso, se você optar por continuar o jogo, vai começar da cidadela. Ou seja, se por algum motivo você fechou o jogo em uma luta ou morreu, vai começar da mesma fase, mas SEM GOLD."
            , "Por fim o que separa uma parte da história de outra são traços na tela: ----, como o que vai aparecer após você apertar Enter.\n"
            , "Bom jogo, e seja bem vindo a Fábulas de Magia : Cidadela de Cristal (FMCC)!"
            ]


inicializaDados::IO()
inicializaDados = do
    let heanes = player
        itens = [item espadaPedra , item armaduraCouro]
        pocaoItem = [pocao cafe]
    salvaPlayer heanes
    salvaItens itens
    salvaPocao pocaoItem
    salvaInimigosFase1

conquistas::String
conquistas = "Conquista {nomeC = \"Jubilado\", descricao = \"Negue Carl Wilson 8 vezes\", alcancou = False}\nConquista {nomeC = \"Se voce nao parar eu Paro\", descricao = \"Tome 6 Pocoes\", alcancou = False}\nConquista {nomeC = \"Faixa Preta\", descricao = \"Derrotou ConversaGPT\", alcancou = False}"

listaDeConquista:: IO [Conquista]
listaDeConquista = return (map parseConquista (lines conquistas))


carregaJogo::IO()
carregaJogo = do
    progresso <- progressoHeroi
    case progresso of
        0 -> do
            putStrLn "Você não tem nenhum jogo salvo, ou você ainda não começou um jogo ou apenas não chegou ao fim do prólogo\nirei começar um jogo agora boa sorte na aventura!"
            esperandoEnter
            comecaJogo
        1 -> resetFase1
        2 -> resetFase2
        3 -> resetFase3
        4 -> fimDeJogo

resetFase1:: IO()
resetFase1 = do
    salvaInimigosFase1
    verificaLoja [item espadaPedra , item armaduraCouro]
    escolhaCaminhoCidade

resetFase2:: IO()
resetFase2 = do
    salvaInimigoFase2
    verificaLoja [item espadaFerro ,item armaduraFerro]
    let pocaoItem = [pocao cafe, pocao redBull]
    salvaPocao pocaoItem
    escolhaCaminhoCidadeFase2

resetFase3::IO()
resetFase3 = do
    salvaInimigoFase3
    verificaLoja [item espadaDiamante, item armaduraDiamante]
    let pocaoItem = [pocao cafe, pocao redBull]
    salvaPocao pocaoItem
    escolhaCaminhoCidadeFase3

verificaLoja:: [Item] -> IO()
verificaLoja loja = do
    heanes <- carregaPlayer
    let itensLoja = removeItensJogadorDeLoja loja (equipamentos heanes)
    salvaItens itensLoja

salvaInimigosFase1::IO()
salvaInimigosFase1 = do
    let dog = inimigo caramelo
        kanvas = inimigo kanva
    salvaInimigo dog (criaCaminho (getNomeInimigo dog))
    salvaInimigo kanvas (criaCaminho (getNomeInimigo kanvas))

salvaInimigoFase2::IO()
salvaInimigoFase2 = do
    let hub = inimigo playHub
    salvaInimigo hub (criaCaminho (getNomeInimigo hub))

salvaInimigoFase3::IO()
salvaInimigoFase3 = do
    let gpt = inimigo conversaGPT
    salvaInimigo gpt (criaCaminho (getNomeInimigo gpt))

removeItensJogadorDeLoja :: [Item] -> [Item] -> [Item]
removeItensJogadorDeLoja = foldr delete