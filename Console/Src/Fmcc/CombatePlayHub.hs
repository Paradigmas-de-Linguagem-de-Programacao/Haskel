module Src.Fmcc.CombatePlayHub where

import Src.Fmcc.Models.Inimigo
import Src.Fmcc.Models.Item
import Src.Fmcc.Models.Player
import Src.Fmcc.Util.Lib
import Src.Fmcc.Util.CombateFuncoes
import Src.Fmcc.Historia
import Src.Fmcc.Historia.Fase3

combatePlayHub :: IO()
combatePlayHub = do
    printString vilaoPlayHub
    printString vilaoPlayHubContinua

    heanes <- carregaPlayer
    putStrLn $ toString heanes
    putStrLn "A fusão das IAs te amedronta, elas começam a te inebriar.\nParece que você realmente não é capaz de ver os status das IA's"
    turnoPreparacao
    turnoAcaoPlayHub

turnoAcaoPlayHub :: IO()
turnoAcaoPlayHub = do
    turnoHeanesPlayHub
    turnoPlayHub
    heanes <- carregaPlayer
    inimigo <- carregaInimigo (criaCaminho "PlayHub")
    if verificaMortoHeroi heanes || verificaMortoInimigo inimigo then do
        if verificaMortoHeroi heanes then morte
        else vitoriaPlayHub
    else turnoAcaoPlayHub

turnoHeanesPlayHub :: IO()
turnoHeanesPlayHub = do
    heanes <- carregaPlayer
    if not (verificaMortoHeroi heanes) then do
        putStrLn "(1)Ataque.\n(2)Usa poção."
        input <- getLine

        if trim input == "1" then do
            usaAtaquePlayHub
            putStrLn "Heanes desfere um ataque crítico e PlayHub perde algumas de suas vozes preferidas."
        else if trim input == "2" then usaPocao
        else do
            putStrLn "Digite uma opção válida."
            turnoHeanesPlayHub
    else putStrLn "Voce errou"

usaAtaquePlayHub :: IO ()
usaAtaquePlayHub = do
    heanes <- carregaPlayer
    inimigo <- carregaInimigo (criaCaminho "PlayHub")
    let ataqueHeanes = Src.Fmcc.Models.Player.ataque heanes
        defesaInimigo = Src.Fmcc.Models.Inimigo.defesa inimigo
        vidaInimigo = Src.Fmcc.Models.Inimigo.vida inimigo
        vidaAtualizadaInimigo = (defesaInimigo + vidaInimigo) - ataqueHeanes
        filepath = criaCaminho (Src.Fmcc.Models.Inimigo.nome inimigo)
        inimigoAtualizado = inimigo {Src.Fmcc.Models.Inimigo.vida = vidaAtualizadaInimigo}
    writeFile filepath (show inimigoAtualizado)

turnoPlayHub :: IO()
turnoPlayHub = do
    inimigo <- carregaInimigo (criaCaminho "PlayHub")
    if not (verificaMortoInimigo inimigo) then do
        if Src.Fmcc.Models.Inimigo.vida inimigo > 35 then do
            ataqueEscolhido <- escolheAtaquePlayHub ["*Voce escuta uma voz...*Heanes: C.W.?\nDistraido, Heanes e atacado furiosamente.", "Voce foi transformado em um PDF e perdeu 3kbs, cuidadoo!", "A fusao das IAs te afunda no chao!!"]
            print ataqueEscolhido
            turnoAtaquePlayHub
        else do
            putStrLn "PlayHub para de brincadeira e utiliza uma mixagem de todas as vozes que pegou para gritar e um som ensurdecedor afeta Heanes criticamente!!!"
            turnoVidaBaixaPlayHub
        heanes <- carregaPlayer
        putStrLn $ toString heanes
    else putStrLn "OH-Ho, você conseguiu héroi!!"

turnoAtaquePlayHub :: IO()
turnoAtaquePlayHub = do
    heanes <- carregaPlayer
    inimigo <- carregaInimigo (criaCaminho "PlayHub")
    let ataqueInimigo = Src.Fmcc.Models.Inimigo.ataque inimigo
        defesaHeanes = Src.Fmcc.Models.Player.defesa heanes
        vidaHeanes = Src.Fmcc.Models.Player.vida heanes
        vidaAtualizadaHeanes = (defesaHeanes + vidaHeanes) - ataqueInimigo
        heanesAtualizado = heanes {Src.Fmcc.Models.Player.vida = vidaAtualizadaHeanes}
    salvaPlayer heanesAtualizado

escolheAtaquePlayHub :: [String] -> IO String
escolheAtaquePlayHub lista = do
    inimigo <- carregaInimigo (criaCaminho "PlayHub")
    let index = Src.Fmcc.Models.Inimigo.vida inimigo `mod` 3
    return (lista !! index)

turnoVidaBaixaPlayHub :: IO()
turnoVidaBaixaPlayHub = do
    heanes <- carregaPlayer
    inimigo <- carregaInimigo (criaCaminho "PlayHub")
    let ataqueInimigo = Src.Fmcc.Models.Inimigo.habilidadeEspecial inimigo
        defesaHeanes = Src.Fmcc.Models.Player.defesa heanes
        vidaHeanes = Src.Fmcc.Models.Player.vida heanes
        vidaAtualizadaHeanes = (defesaHeanes + vidaHeanes) - ataqueInimigo
        heanesAtualizado = heanes {Src.Fmcc.Models.Player.vida = vidaAtualizadaHeanes}
    salvaPlayer heanesAtualizado

vitoriaPlayHub::IO()
vitoriaPlayHub = do
    printString vitoriaPlayHubDialogo
    printString vitoriaPlayHubSaida
    clearScreen
    comecaFase3
    escolhaCaminhoCidadeFase3