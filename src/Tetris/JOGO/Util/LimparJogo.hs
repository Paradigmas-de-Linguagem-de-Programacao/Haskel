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

limpaLinha :: [Int] -> [Int]
limpaLinha linha 
    | 0 `elem` linha = linha
    | otherwise = [0 | _ <- [1..10]]

limparLinhas :: [[Int]] -> Int -> [[Int]]
limparLinhas grid 19 = [limpaLinha (grid !! 19)] 
limparLinhas grid altura = limpaLinha (grid !! altura) : limparLinhas grid (altura + 1)

clearGame :: [[Int]] -> [[Int]]
clearGame grid = shiftBaixo (limparLinhas grid 0) 0