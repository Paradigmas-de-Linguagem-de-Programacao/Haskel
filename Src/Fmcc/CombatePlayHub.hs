module Fmcc.CombatePlayHub where

import Fmcc.Models.Inimigo
import Fmcc.Models.Item
import Fmcc.Models.Player
import Fmcc.Util.Lib
import Fmcc.Util.CombateFuncoes
import Fmcc.Historia
import Fmcc.Historia.Fase3

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
    let ataqueHeanes = Fmcc.Models.Player.ataque heanes
        defesaInimigo = Fmcc.Models.Inimigo.defesa inimigo
        vidaInimigo = Fmcc.Models.Inimigo.vida inimigo
        vidaAtualizadaInimigo = (defesaInimigo + vidaInimigo) - ataqueHeanes
        filepath = criaCaminho (Fmcc.Models.Inimigo.nome inimigo)
        inimigoAtualizado = inimigo {Fmcc.Models.Inimigo.vida = vidaAtualizadaInimigo}
    writeFile filepath (show inimigoAtualizado)

turnoPlayHub :: IO()
turnoPlayHub = do
    inimigo <- carregaInimigo (criaCaminho "PlayHub")
    if not (verificaMortoInimigo inimigo) then do
        if Fmcc.Models.Inimigo.vida inimigo > 35 then do
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
    let ataqueInimigo = Fmcc.Models.Inimigo.ataque inimigo
        defesaHeanes = Fmcc.Models.Player.defesa heanes
        vidaHeanes = Fmcc.Models.Player.vida heanes
        vidaAtualizadaHeanes = (defesaHeanes + vidaHeanes) - ataqueInimigo
        heanesAtualizado = heanes {Fmcc.Models.Player.vida = vidaAtualizadaHeanes}
    salvaPlayer heanesAtualizado

escolheAtaquePlayHub :: [String] -> IO String
escolheAtaquePlayHub lista = do
    inimigo <- carregaInimigo (criaCaminho "PlayHub")
    let index = Fmcc.Models.Inimigo.vida inimigo `mod` 3
    return (lista !! index)

turnoVidaBaixaPlayHub :: IO()
turnoVidaBaixaPlayHub = do
    heanes <- carregaPlayer
    inimigo <- carregaInimigo (criaCaminho "PlayHub")
    let ataqueInimigo = Fmcc.Models.Inimigo.habilidadeEspecial inimigo
        defesaHeanes = Fmcc.Models.Player.defesa heanes
        vidaHeanes = Fmcc.Models.Player.vida heanes
        vidaAtualizadaHeanes = (defesaHeanes + vidaHeanes) - ataqueInimigo
        heanesAtualizado = heanes {Fmcc.Models.Player.vida = vidaAtualizadaHeanes}
    salvaPlayer heanesAtualizado

vitoriaPlayHub::IO()
vitoriaPlayHub = do
    printString vitoriaPlayHubDialogo
    printString vitoriaPlayHubSaida
    clearScreen
    comecaFase3
    escolhaCaminhoCidadeFase3