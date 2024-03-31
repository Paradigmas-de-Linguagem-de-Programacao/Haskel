module CombateKanva where

import Historia
import Util.Lib
import Models.Player
import Models.Pocao
import System.IO
import Util.CombateFuncoes
import Models.Inimigo
import Historia.Fase2 (escolhaCaminhoCidadeFase2)


combateKanva :: IO()
combateKanva = do
    putStrLn (textoFormatado "*Sem tempo para comemorar a vitória você é puxado para dentro do museu deixando Leandro para trás.*\n")
    esperandoEnter
    clearScreen
    putStrLn vilaoKanva
    esperandoEnter
    clearScreen
    putStrLn "Se prepare rapidamente para o combate!.\n"
    putStrLn "Esses são seus status atuais, mas C.W. te acompanhou de longe e liberou uma poção a mais no seu inventário, essa poção é uma nova criação do Mestre dos Magos, e não pode ser comprada.\n"

    adicionaPocaoCW
    heanes <- carregaPlayer
    print heanes
    putStrLn (textoFormatado "")
    esperandoEnter
    clearScreen

    putStrLn "Kanva finalmente notou que você se aproxima para o combate!!!\n\nTOME SUA DECISÃO HÉROI!!\n"
    turnoAcaoKanva

adicionaPocaoCW :: IO()
adicionaPocaoCW = do
    heroi <- carregaPlayer
    arquivoPocao <- readFile' "./src/pacote/PocaoMonster.txt"
    let pocaoMonster = read arquivoPocao :: Pocao
        pocoesAtualizada = Models.Player.pocoes heroi ++ [pocaoMonster]
        heanesAtualizado = heroi {Models.Player.pocoes = pocoesAtualizada}
    salvaPlayer heanesAtualizado

turnoAcaoKanva :: IO()
turnoAcaoKanva = do
    turnoHeanesKanva
    turnoKanva
    heanes <- carregaPlayer
    inimigo <- carregaInimigo (criaCaminho "Kanva")
    if verificaMortoHeroi heanes || verificaMortoInimigo inimigo then do
        if verificaMortoHeroi heanes then morte
        else do
            vitoriaKanva
    else turnoAcaoKanva

turnoHeanesKanva :: IO()
turnoHeanesKanva = do
    heanes <- carregaPlayer
    if not (verificaMortoHeroi heanes) then do
        putStrLn "(1) Ataque.\n(2) Usa poção.\n"
        putStrLn "------------------------------------------------------------------------------------\n"
        input <- trim <$> getLine
        if input == "1" then do
            usaAtaqueKanva
            putStrLn "Com sua espada fervorosa você fere o Kanva destruindo alguns de seus grandes tentáculos-pincéis."
        else if input == "2" then usaPocao
        else do
            putStrLn "Digite uma opção válida."
            turnoHeanesKanva
    else putStrLn "Oh não dog, você..."

usaAtaqueKanva :: IO ()
usaAtaqueKanva = do
    heanes <- carregaPlayer
    inimigo <- carregaInimigo (criaCaminho "Kanva")
    let ataqueHeanes = Models.Player.ataque heanes
        defesaInimigo = Models.Inimigo.defesa inimigo
        vidaInimigo = Models.Inimigo.vida inimigo
        vidaAtualizadaInimigo = (defesaInimigo + vidaInimigo) - ataqueHeanes
        filepath = criaCaminho (Models.Inimigo.nome inimigo)
        inimigoAtualizado = inimigo {Models.Inimigo.vida = vidaAtualizadaInimigo}
    salvaInimigo inimigoAtualizado filepath

turnoKanva :: IO()
turnoKanva = do
    inimigo <- carregaInimigo (criaCaminho "Kanva")
    if not (verificaMortoInimigo inimigo) then do
        if Models.Inimigo.vida inimigo > 35 then do
            ataqueEscolhido <- escolheAtaqueKanva ["Kanva desenha uma bola de fogo indo na sua direcao", "Repentinamente varias telas saltam sobre voce!! CUIDADO!", "Kanva joga varios pinceis enraivados contra voce!!"]
            print ataqueEscolhido
            turnoAtaqueKanva
        else do
            putStrLn "Kanvas se enfurece cada vez mais e utiliza sua habilidade especial!! O dano dele é aumentado!\n Não sei qual vai ser, mudem aqui."
            turnoVidaBaixaKanva
        heanes <- carregaPlayer
        print (toString heanes)
    else putStrLn "O Kanva foi derrotado, PARABÉNS HERÓI!!!! A CIDADE COMEMORA POR VOCÊ."

turnoAtaqueKanva :: IO()
turnoAtaqueKanva = do
    heanes <- carregaPlayer
    inimigo <- carregaInimigo (criaCaminho "Kanva")
    let ataqueInimigo = Models.Inimigo.ataque inimigo
        defesaHeanes = Models.Player.defesa heanes
        vidaHeanes = Models.Player.vida heanes
        vidaAtualizadaHeanes = (defesaHeanes + vidaHeanes) - ataqueInimigo
        heanesAtualizado = heanes {Models.Player.vida = vidaAtualizadaHeanes}
    salvaPlayer heanesAtualizado

escolheAtaqueKanva :: [String] -> IO String
escolheAtaqueKanva lista = do
    inimigo <- carregaInimigo (criaCaminho "Kanva")
    let index = Models.Inimigo.vida inimigo `mod` 3
    return (lista !! index)

turnoVidaBaixaKanva :: IO()
turnoVidaBaixaKanva = do
    heanes <- carregaPlayer
    inimigo <- carregaInimigo (criaCaminho "Kanva")
    let ataqueInimigo = Models.Inimigo.habilidadeEspecial inimigo
        defesaHeanes = Models.Player.defesa heanes
        vidaHeanes = Models.Player.vida heanes
        vidaAtualizadaHeanes = (defesaHeanes + vidaHeanes) - ataqueInimigo
        heanesAtualizado = heanes {Models.Player.vida = vidaAtualizadaHeanes}
    salvaPlayer heanesAtualizado


vitoriaKanva::IO()
vitoriaKanva = do
    printString vitoriaKanvaDialogo
    printString vitoriaKanvaSaida
    comecaFase2
    escolhaCaminhoCidadeFase2
