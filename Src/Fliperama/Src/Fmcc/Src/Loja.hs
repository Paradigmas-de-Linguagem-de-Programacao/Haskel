module Loja where
import Models.Player
import Models.Item
import Historia ()
import Util.Lib
import System.IO (readFile')
import Models.Pocao

abreLojaItens :: IO()
abreLojaItens = do
    clearScreen
    putStrLn (textoFormatado "Ferreira, o ferreiro: Olá herói! Aqui está uma lista de itens que você pode comprar caso tenha as moedas, claro:\n")
    arquivo02 <- readFile' itemCaminho
    let lojaItens = map (read::String->Item) (lines arquivo02)
    putStrLn $ (toStringItensLoja lojaItens)
    putStrLn (textoFormatado "\nDeseja comprar algo?\n\n(1) Sim.\n(2) Não.\n")
    input <- getLine
    if trim input == "1" then compraItem lojaItens
    else do
        clearScreen
        putStrLn (textoFormatado("Não quer comprar hein... Tudo bem.\n"))
        esperandoEnter

abreLojaPocoes:: IO()
abreLojaPocoes = do
    clearScreen

    putStrLn (textoFormatado "Olá herói! Aqui está uma lista de poções que você pode comprar caso tenha as moedas, claro:\n")
    arquivoPocao <- readFile' pocaoCaminho
    let lojaPocao = map (read::String->Pocao) (lines arquivoPocao)
    putStrLn(toStringPocaoLoja lojaPocao)
    putStrLn (textoFormatado "\nDeseja comprar algo?\n\n(1) Sim.\n(2) Não.\n")
    input <- getLine
    if trim input == "1" then compraPocao lojaPocao
    else do
        clearScreen
        putStrLn (textoFormatado("Não quer comprar hein... Tudo bem.\n"))
        esperandoEnter

compraItem::[Item]->IO()
compraItem lojaItens = do
    heanesPre <- carregaPlayer
    putStrLn $ "\nDigite o nome do item que você deseja comprar.\n\n" ++ "Esse é o seu Gold: " ++ show (getGold heanesPre) ++ "\n"

    input <- getLine
    let maybeItem = identificaItem input lojaItens
    case maybeItem of
        Just item -> do
            let gold01 = getGold heanesPre
                precoItem = getPrecoItem item
            if gold01 >= precoItem then do
                let goldAtual = gold01 - precoItem
                    listaItensAtualizada = equipamentos heanesPre ++ [item]
                    heanesAdulto = heanesPre { gold = goldAtual, equipamentos = listaItensAtualizada}
                    listaAtualItens = removeItem input lojaItens
                salvaPlayer heanesAdulto
                salvaItens listaAtualItens
                clearScreen
                putStrLn(textoFormatado("Compra realizada com sucesso.\n"))
                esperandoEnter
                clearScreen
            else do
                clearScreen
                putStrLn (textoFormatado("Está pobre, tente novamente\n"))
                esperandoEnter
                abreLojaItens
        Nothing -> do
            clearScreen
            putStrLn (textoFormatado("Por favor tente novamente.\n"))
            esperandoEnter
            abreLojaItens


compraPocao::[Pocao] -> IO()
compraPocao lojaPocao = do
    putStrLn "\nDigite o nome da pocao que você deseja comprar.\n"
    pocaoNome <- getLine
    let maybePocao = identificaPocao pocaoNome lojaPocao
    case maybePocao of
        Just pocao -> do
            heanesPre <- carregaPlayer
            let gold = getGold heanesPre
                precoPocao = getPrecoPocao pocao
            if gold >= precoPocao then
                if identificaPocaoJaComprada pocaoNome (pocoes heanesPre) then do
                    let goldAtual = gold - precoPocao
                        pocaoInicial = pegaPocao pocaoNome (pocoes heanesPre)
                        quantidadeAtual = getQuantidadePocao pocaoInicial + 1
                        pocaoFinal = pocaoInicial {quantidade = quantidadeAtual}
                        listaPocoesAtualizada = removePocaoAntiga pocaoNome (pocoes heanesPre) ++ [pocaoFinal]
                        heanesAdulto = heanesPre { gold = goldAtual, pocoes = listaPocoesAtualizada}
                    salvaPlayer heanesAdulto
                    clearScreen
                    putStrLn(textoFormatado("Compra realizada com sucesso.\n"))
                    esperandoEnter
                else do
                    let goldAtual = gold - precoPocao
                        heanesAdulto = heanesPre { gold = goldAtual, pocoes = pocoes heanesPre ++ [pocao] }
                    salvaPlayer heanesAdulto
                    clearScreen
                    putStrLn (textoFormatado("Compra realizada com sucesso.\n"))
                    esperandoEnter
            else do
                clearScreen
                putStrLn (textoFormatado("Está pobre\n"))
                esperandoEnter
                abreLojaPocoes
        Nothing -> do
            clearScreen
            putStrLn (textoFormatado("Por favor tente novamente\n"))
            esperandoEnter
            abreLojaPocoes

verLoja::IO()
verLoja = do
    putStrLn "Ferreira, o ferreiro: Olá Herói! Esses são os itens e os preços que se você tiver dinheiro, poderá comprar: \n"
    arquivo02 <- readFile' itemCaminho
    let loja = map (read::String->Item) (lines arquivo02)
    putStrLn $ (toStringItensLoja loja)

