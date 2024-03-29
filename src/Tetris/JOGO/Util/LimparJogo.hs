-- 1. Jogo rodar em uma grid 21x10 (dimensão válida ser 20x10, somada a uma dimensão externa acima para derrota do jogador)
-- 2. Existir funções para limpeza de linhas, além de shift para baixo
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

module Util.LimparJogo (clearGame) where

verificaShift :: [Int] -> Bool
verificaShift [] = True
verificaShift (tipo : rabo) =
    tipo == 0 && verificaShift rabo

trocaSucessores :: [[Int]] -> Int -> [[Int]]
trocaSucessores linha i =
    take i linha ++ [linha !! (i+1)] ++ [linha !! i] ++ drop (i + 2) linha

shiftBaixoLinha :: [[Int]] -> Int -> [[Int]]
shiftBaixoLinha grid 0 = grid
shiftBaixoLinha grid indice
    | verificaShift (grid !! (indice - 1)) = shiftBaixoLinha (trocaSucessores grid (indice-1)) (indice - 1)
    | otherwise = grid

shiftBaixo :: [[Int]] -> Int -> [[Int]]
shiftBaixo grid 20 = grid
shiftBaixo grid indice = shiftBaixo (shiftBaixoLinha grid indice) (indice + 1)

limpaLinha :: [Int] -> ([Int], Bool)
limpaLinha linha
    | ehTroca = ([0 | _ <- [1..10]], ehTroca) 
    | otherwise = (linha, ehTroca)
    where
        ehTroca =  0 `notElem` linha

limparLinhas :: [[Int]] -> Int -> ([[Int]], Int)
limparLinhas grid 19 = ([linha], quantidadeLinha)
    where
        quantidadeLinha = if ehTrocado then 1 else 0
        (linha, ehTrocado) = limpaLinha (grid !! 19)
limparLinhas grid altura = (linha: linhas, quantidadeLinha + linhasTrocadas)
    where
        quantidadeLinha = if ehTrocado then 1 else 0
        (linha, ehTrocado) = limpaLinha (grid !! altura)
        (linhas, linhasTrocadas) = limparLinhas grid (altura + 1)


clearGame :: [[Int]] -> ([[Int]], Int)
clearGame grid = (shiftBaixo novaGrid 0, qtdLinhasLimpas)
    where
        (novaGrid, qtdLinhasLimpas) = limparLinhas grid 0