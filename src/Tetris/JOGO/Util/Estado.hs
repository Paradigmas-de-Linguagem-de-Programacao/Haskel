{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Util.Estado (
    Estado(..), Peca(..), geraPeca, geraEstadoInicial, atribuicaoPeca, verificaAtribuicaoPeca
) where

data Estado = Estado {
    grid :: [[Int]],
    linhas :: Int,
    nivel :: Int,
    tempo :: Int,
    pontuacao :: Int,
    atualPeca :: Peca,
    proximaPeca :: Peca,
    jogoAcabou :: Bool
} deriving (Show)

data Peca = Peca {
    qualPeca :: Int,
    formatosPeca :: [[[Int]]],
    atualEstado :: Int,
    coordenadas :: ((Int, Int), (Int, Int))
} deriving (Show)

geraEstadoInicial :: Estado
geraEstadoInicial = Estado {
    grid = atribuicaoPeca [[0 | _ <- [1..10]] | _ <- [1..20]] (head geraI),
    linhas = 0,
    nivel = 1,
    tempo = 0,
    pontuacao = 0,
    atualPeca =  geraPeca 0,
    proximaPeca = geraPeca 1,
    jogoAcabou = False
    }
insereElementoLista :: [t] -> Int -> t -> [t]
insereElementoLista [] _ _ = []
insereElementoLista lista indice elemento = take indice lista ++ [elemento] ++ drop (indice + 1) lista 

atribuicaoPeca :: [[Int]] -> [[Int]] -> [[Int]]
atribuicaoPeca grid peca 
    | tamanhoPeca == 1 = insereElementoLista grid 19 (atribuicaoPecaRecursiva  indices (last grid) (head peca))
    | otherwise = insereElementoLista (insereElementoLista grid 19 (atribuicaoPecaRecursiva  indices (last grid) (head peca))) 18 (atribuicaoPecaRecursiva  indices (grid !! 18)  (last peca))
    where 
        tamanhoPeca = length peca
        indices = mapeiaColunas (head peca)

atribuicaoPecaRecursiva :: [Int] -> [Int] -> [Int] -> [Int]
atribuicaoPecaRecursiva [] grid [] = grid
atribuicaoPecaRecursiva (i:is) grid (a:as) = insereElementoLista (atribuicaoPecaRecursiva is grid as) i a 

verificaAtribuicaoPeca :: [[Int]] -> [[Int]] -> Bool
verificaAtribuicaoPeca grid peca 
    | linhasPeca == 1 = verificaAtribuicaoPecaRecursiva colunas (grid !! 19) (head peca)
    | otherwise = verificaAtribuicaoPecaRecursiva colunas (grid !! 19) (head peca) && verificaAtribuicaoPecaRecursiva colunas (grid !! 19) (peca !! 1)
    where
        linhasPeca = length peca
        colunas = mapeiaColunas (head peca)

verificaAtribuicaoPecaRecursiva :: [Int] -> [Int] -> [Int] -> Bool  
verificaAtribuicaoPecaRecursiva [] _ [] = True
verificaAtribuicaoPecaRecursiva (x:xs) linha (a:as) = ((linha !! x) == 0 || a == 0) && verificaAtribuicaoPecaRecursiva xs linha as

mapeiaColunas :: [Int] -> [Int]
mapeiaColunas formatoPeca = [menorIndice..maiorIndice]
    where
        tamanhoPeca = length formatoPeca
        menorIndice = if even tamanhoPeca then 5 - (tamanhoPeca `div` 2) else 5 - (tamanhoPeca `div` 2) - 1
        maiorIndice = 5 + (tamanhoPeca `div` 2) - 1

geraPeca :: Int -> Peca
geraPeca indice = Peca {qualPeca = indice, formatosPeca = formato, coordenadas = ((x1,y1), (x2,y2)), atualEstado = 0}
    where
        funcaoGeradora = [geraI, geraL, geraO, geraR, geraS, geraT, geraZ]
        formato = funcaoGeradora !! indice
        formatoInicial = head formato
        indices = mapeiaColunas (head formatoInicial)
        alturaFormato = length formatoInicial
        (y1, y2) = (19, 19 - (alturaFormato - 1))
        (x1, x2) = (head indices, last indices)

geraI :: [[[Int]]]
geraI = formatos
    where 
        formatos = [
            [[1, 1, 1, 1]],
            [[1],[1],[1],[1]],
            [[1, 1, 1, 1]],
            [[1],[1],[1],[1]]
            ] 

geraL :: [[[Int]]]
geraL = formatos
    where 
        formatos = [
            [[2, 2, 2],[2, 0, 0]],
            [[2, 0],[2, 0],[2, 2]],
            [[0, 0, 2],[2, 2, 2]],
            [[2, 2],[0, 2],[0, 2]]
            ]

geraO :: [[[Int]]]
geraO = formatos
    where
        formatos = [
            [[3, 3],[3, 3]],
            [[3, 3],[3, 3]],
            [[3, 3],[3, 3]],
            [[3, 3],[3, 3]]
            ]

geraR :: [[[Int]]]
geraR = formatos
    where 
        formatos = [
            [[4, 4, 4],[0, 0, 4]],
            [[4,4],[4,0],[4,0]],
            [[4, 0, 0],[4, 4, 4]],
            [[0,4],[0,4],[4,4]]
            ]

geraS :: [[[Int]]]
geraS = formatos
    where formatos = [
            [[5, 5, 0],[0, 5, 5]],
            [[0, 5],[5, 5],[5, 0]],
            [[5, 5, 0],[0, 5, 5]],
            [[0, 5],[5, 5],[5, 0]]
            ]

geraT :: [[[Int]]]
geraT = formatos
    where formatos = [
            [[6, 6, 6],[0, 6, 0]],
            [[0, 6],[6, 6],[0, 6]],
            [[0, 6, 0],[6, 6, 6]],
            [[6,0],[6,6],[6,0]]
            ]

geraZ :: [[[Int]]]
geraZ = formatos
    where formatos = [
            [[0, 7, 7],[7, 7, 0]],
            [[7,0],[7,7],[0,7]],
            [[0, 7, 7],[7, 7, 0]],
            [[7,0],[7,7],[0,7]]
            ]