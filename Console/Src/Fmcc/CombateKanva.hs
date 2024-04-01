module Src.Fmcc.CombateKanva where

import Src.Fmcc.Historia
import Src.Fmcc.Util.Lib
import Src.Fmcc.Models.Player
import Src.Fmcc.Models.Pocao
import System.IO
import Src.Fmcc.Util.CombateFuncoes
import Src.Fmcc.Models.Inimigo
import Src.Fmcc.Historia.Fase2 (escolhaCaminhoCidadeFase2)


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
    putStrLn $ toString heanes
    putStrLn (textoFormatado "")
    esperandoEnter
    clearScreen

    putStrLn "Kanva finalmente notou que você se aproxima para o combate!!!\nVocê percebe que não consegue ver os status das IA, a luta se torna mais complexa\nTOME SUA DECISÃO HÉROI!!\n\n"

    turnoAcaoKanva

adicionaPocaoCW :: IO()
adicionaPocaoCW = do
    heroi <- carregaPlayer
    arquivoPocao <- readFile "./Src/Fmcc/pacote/PocaoMonster.txt"
    let pocaoMonster = read arquivoPocao :: Pocao
        pocoesAtualizada = Src.Fmcc.Models.Player.pocoes heroi ++ [pocaoMonster]
        heanesAtualizado = heroi {Src.Fmcc.Models.Player.pocoes = pocoesAtualizada}
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
    let ataqueHeanes = Src.Fmcc.Models.Player.ataque heanes
        defesaInimigo = Src.Fmcc.Models.Inimigo.defesa inimigo
        vidaInimigo = Src.Fmcc.Models.Inimigo.vida inimigo
        vidaAtualizadaInimigo = (defesaInimigo + vidaInimigo) - ataqueHeanes
        filepath = criaCaminho (Src.Fmcc.Models.Inimigo.nome inimigo)
        inimigoAtualizado = inimigo {Src.Fmcc.Models.Inimigo.vida = vidaAtualizadaInimigo}
    salvaInimigo inimigoAtualizado filepath

turnoKanva :: IO()
turnoKanva = do
    inimigo <- carregaInimigo (criaCaminho "Kanva")
    if not (verificaMortoInimigo inimigo) then do
        if Src.Fmcc.Models.Inimigo.vida inimigo > 35 then do
            ataqueEscolhido <- escolheAtaqueKanva ["Kanva desenha uma bola de fogo indo na sua direcao", "Repentinamente varias telas saltam sobre voce!! CUIDADO!", "Kanva joga varios pinceis enraivados contra voce!!"]
            print ataqueEscolhido
            turnoAtaqueKanva
        else do
            putStrLn "Kanvas se enfurece cada vez mais e utiliza sua habilidade especial!! O dano dele é aumentado!\n Não sei qual vai ser, mudem aqui."
            turnoVidaBaixaKanva
        heanes <- carregaPlayer
        putStrLn $ toString heanes
    else putStrLn "O Kanva foi derrotado, PARABÉNS HERÓI!!!! A CIDADE COMEMORA POR VOCÊ."

turnoAtaqueKanva :: IO()
turnoAtaqueKanva = do
    heanes <- carregaPlayer
    inimigo <- carregaInimigo (criaCaminho "Kanva")
    let ataqueInimigo = Src.Fmcc.Models.Inimigo.ataque inimigo
        defesaHeanes = Src.Fmcc.Models.Player.defesa heanes
        vidaHeanes = Src.Fmcc.Models.Player.vida heanes
        vidaAtualizadaHeanes = (defesaHeanes + vidaHeanes) - ataqueInimigo
        heanesAtualizado = heanes {Src.Fmcc.Models.Player.vida = vidaAtualizadaHeanes}
    salvaPlayer heanesAtualizado

escolheAtaqueKanva :: [String] -> IO String
escolheAtaqueKanva lista = do
    inimigo <- carregaInimigo (criaCaminho "Kanva")
    let index = Src.Fmcc.Models.Inimigo.vida inimigo `mod` 3
    return (lista !! index)

turnoVidaBaixaKanva :: IO()
turnoVidaBaixaKanva = do
    heanes <- carregaPlayer
    inimigo <- carregaInimigo (criaCaminho "Kanva")
    let ataqueInimigo = Src.Fmcc.Models.Inimigo.habilidadeEspecial inimigo
        defesaHeanes = Src.Fmcc.Models.Player.defesa heanes
        vidaHeanes = Src.Fmcc.Models.Player.vida heanes
        vidaAtualizadaHeanes = (defesaHeanes + vidaHeanes) - ataqueInimigo
        heanesAtualizado = heanes {Src.Fmcc.Models.Player.vida = vidaAtualizadaHeanes}
    salvaPlayer heanesAtualizado


vitoriaKanva::IO()
vitoriaKanva = do
    printString vitoriaKanvaDialogo
    printString vitoriaKanvaSaida
    clearScreen
    comecaFase2
    escolhaCaminhoCidadeFase2
