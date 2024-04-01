module Src.Fmcc.Combate where
import Src.Fmcc.Util.Lib
import Src.Fmcc.Util.CombateFuncoes
import Src.Fmcc.Historia
import Src.Fmcc.Models.Player
import Src.Fmcc.Models.Inimigo
import System.IO
import Src.Fmcc.CombateKanva

explicacaoCombate01 :: String
explicacaoCombate01 = textoFormatado "Você estará entrando em combate em breve. Suas jogadas serão definidas em turnos intercalados entre você e seu inimigo, por isso, tome bastante cuidado nas suas decisões.\n"

combate01 :: IO()
combate01 = do
    corrigeMonster
    mapM_ printString [kanvaHistoria1, kanvaHistoria2 , kanvaHistoria3, kanvaHistoria4,kanvaHistoria5,kanvaHistoria6
        , kanvaHistoria7, caramelosKanva, explicacaoCombate01]
    clearScreen
    putStrLn "Dê uma olhada nos seus status e nos status de seu inimigo. Quando utilizar um item ou poção os atributos vão ser adicionados aos seus status básicos. Quando você utiliza um equipamento ou poção, ele é descartado após o combate, logo, tenha cuidado no que vai usar.\n"

    arquivoCaramelos <- readFile "./Src/Fmcc/pacote/Cachorros Caramelos.txt"
    let caramelos = read arquivoCaramelos :: Inimigo
    putStrLn "Status dos cachorros:"
    putStrLn $ toStringInimigo caramelos
    putStrLn (textoFormatado(""))
    heanes <- carregaPlayer
    putStrLn "Seus Status:"
    putStrLn  $ toString heanes
    putStrLn ( textoFormatado "\nVocê terá 2 turnos, um de preparo e outro que vai ser seguido pelo ataque dos caramelos. Prepare-se antes que os caramelinhos morda você!\n")
    esperandoEnter
    clearScreen
    turnoPreparacao
    clearScreen
    turnoAcao01

corrigeMonster :: IO()
corrigeMonster = do
    heroi <- carregaPlayer
    let maybePocao = identificaPocao "Monster" (Src.Fmcc.Models.Player.pocoes heroi)
    case maybePocao of
        Just pocao -> do
            let pocoesAtualizada = removePocaoAntiga "Monster" (Src.Fmcc.Models.Player.pocoes heroi)
                heanesMonster = heroi {Src.Fmcc.Models.Player.pocoes = pocoesAtualizada}
            salvaPlayer heanesMonster
        Nothing -> putStrLn ""

turnoAcao01 :: IO()
turnoAcao01 = do
    turnoHeanesCaramelo
    turnoCaramelo
    heanes <- carregaPlayer
    inimigo <- carregaInimigo (criaCaminho "Cachorros Caramelos")
    if verificaMortoHeroi heanes || verificaMortoInimigo inimigo then do
        if verificaMortoHeroi heanes then morte
        else combateKanva
    else turnoAcao01

turnoHeanesCaramelo :: IO()
turnoHeanesCaramelo = do
    clearScreen
    heanes <- carregaPlayer
    if not (verificaMortoHeroi heanes) then do
        putStrLn "Escolha uma ação:\n"
        putStrLn (textoFormatado "(1) Ataque.\n(2) Usa poção.\n")
        input <- trim <$>getLine

        if input == "1" then do
            clearScreen
            usaAtaque
            putStrLn "Voce desfere um ataque fatal a alguns cachorros que o cercavam. O IBAMA agora sabe onde você mora.\n"
            inimigo <- carregaInimigo (criaCaminho "Cachorros Caramelos")
            putStrLn $ toStringInimigo inimigo
            putStrLn "\n------------------------------------------------------------------------------------\n"
            esperandoEnter
        else if input == "2" then do
            clearScreen
            usaPocao
        else do
            putStrLn "\nDigite uma opção válida."
            turnoHeanesCaramelo
    else putStrLn "Voce morreu..."

usaAtaque :: IO()
usaAtaque = do
    heanes <- carregaPlayer
    inimigo <- carregaInimigo (criaCaminho "Cachorros Caramelos")
    let ataqueHeanes = Src.Fmcc.Models.Player.ataque heanes
        defesaInimigo = Src.Fmcc.Models.Inimigo.defesa inimigo
        vidaInimigo = Src.Fmcc.Models.Inimigo.vida inimigo
        vidaAtualizadaInimigo = (defesaInimigo + vidaInimigo) - ataqueHeanes
        filepath = criaCaminho (Src.Fmcc.Models.Inimigo.nome inimigo)
        inimigoAtualizado = inimigo {Src.Fmcc.Models.Inimigo.vida = vidaAtualizadaInimigo}
    writeFile filepath (show inimigoAtualizado)


turnoCaramelo :: IO()
turnoCaramelo = do
    clearScreen
    inimigo <- carregaInimigo (criaCaminho "Cachorros Caramelos")
    if not (verificaMortoInimigo inimigo) then do
        putStrLn "UM CARAMELINHO TE MORDE VIOLENTAMENTE E VOCÊ GRITA: SAIII DOGGG!!!\n"
        turnoAtaqueCaramelo
        heanes <- carregaPlayer
        putStrLn $ toString heanes
        putStrLn "\n------------------------------------------------------------------------------------\n"
        esperandoEnter
    else putStrLn "Leandro: Você conseguiu! Matou todos os cachorros mágicos, eu sabia que você era forte.\n"

turnoAtaqueCaramelo :: IO()
turnoAtaqueCaramelo = do
    heanes <- carregaPlayer
    inimigo <- carregaInimigo (criaCaminho "Cachorros Caramelos")
    let ataqueInimigo = Src.Fmcc.Models.Inimigo.ataque inimigo
        defesaHeanes = Src.Fmcc.Models.Player.defesa heanes
        vidaHeanes = Src.Fmcc.Models.Player.vida heanes
        vidaAtualizadaHeanes = (defesaHeanes + vidaHeanes) - ataqueInimigo
        heanesAtualizado = heanes {Src.Fmcc.Models.Player.vida = vidaAtualizadaHeanes}
    salvaPlayer heanesAtualizado
