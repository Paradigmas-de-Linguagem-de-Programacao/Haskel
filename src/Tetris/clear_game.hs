-- 1. Jogo rodar em uma grid 21x10 (dimensão válida ser 20x10, somada a uma dimensão externa acima para derrota do jogador)
-- 2. Existir funções para limpeza de linhas, além de shift para baixo

verifica_shift :: [(Int, Int)] -> Bool
verifica_shift [] = True
verifica_shift ((tipo, _): rabo) = tipo == 0 && verifica_shift rabo

troca_sucessores :: [[(Int, Int)]] -> Int -> [(Int, Int)]
troca_sucessores linha i = ((take i linha) ++ [linha !! (i+1)] ++ [linha !! i] ++ (drop (i + 2) linha)) 

shift_baixo_linha :: [[(Int, Int)]] -> Int -> [[(Int, Int)]]
shift_baixo_linha grid 0 = grid
shift_baixo_linha grid indice
    | verifica_shift (grid !! (indice - 1)) = shift_baixo_linha (troca_sucessores grid indice) (indice - 1)
    | otherwise = grid

shift_baixo :: [[(Int, Int)]] -> Int -> [[(Int, Int)]]
shift_baixo grid 20 = grid
shift_baixo grid indice = shift_baixo (shift_baixo_linha indice) (indice + 1)

limpa_linha :: [(Int, Int)] -> [(Int, Int)]
limpa_linha linha 
    | elem (0,0) linha = linha
    | otherwise = [(0, 0) | _ <- [1..10]]

limpar_linhas :: [[(Int, Int)]] -> Int -> [[(Int, Int)]]
limpar_linhas grid 19 = (limpa_linha grid !! 19) : (grid !! 20) -- O índice 20 precisa se manter
limpar_linhas grid altura = (limpa_linha grid altura) : (limpar_linhas grid (altura + 1))

clear_game :: [(Int,Int)] -> [(Int,Int)]
clear_game grid = shift_baixo (limpar_linhas grid 0) 0