{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Util.Estado (
    Estado(..), EstatisticaPeca(..), Peca(..), geraPeca, geraEstadoInicial, atribuicaoPeca
) where

data Estado = Estado {
    grid :: [[Int]],
    linhas :: Int,
    nivel :: Int,
    pontuacao :: Int,
    atualPeca :: Peca,
    proximaPeca :: Peca,
    estatisticas :: [EstatisticaPeca]
} deriving (Show)

data EstatisticaPeca = EstatisticaPeca {
    peca :: Peca,
    quantidade :: Int
} deriving (Show)

data Peca = Peca {
    qualPeca :: Int,
    formatoPeca :: [[Int]]
} deriving (Show)

geraEstadoInicial :: Estado
geraEstadoInicial = Estado {
    grid = atribuicaoPeca [[0 | _ <- [1..10]] | _ <- [1..20]] geraI,
    linhas = 0,
    nivel = 0,
    pontuacao = 0,
    atualPeca =  geraPeca 0,
    proximaPeca = geraPeca 1,
    estatisticas = [
        EstatisticaPeca {
            peca = geraPeca 0,
            quantidade = 0
        },
        EstatisticaPeca {
            peca = geraPeca 1,
            quantidade = 0
        },
        EstatisticaPeca {
            peca = geraPeca 2,
            quantidade = 0
        },
        EstatisticaPeca {
            peca = geraPeca 3,
            quantidade = 0
        },
        EstatisticaPeca {
            peca = geraPeca 4,
            quantidade = 0
        },
        EstatisticaPeca {
            peca = geraPeca 5,
            quantidade = 0
        },
        EstatisticaPeca {
            peca = geraPeca 6,
            quantidade = 0
        }
    ]
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
    | linhasPeca == 1 = verificaAtribuicaoPecaRecursiva colunas (grid !! 20) (head peca)
    | otherwise = verificaAtribuicaoPecaRecursiva colunas (grid !! 20) (head peca) && verificaAtribuicaoPecaRecursiva colunas (grid !! 19) (peca !! 1)
    where
        linhasPeca = length peca
        colunas = mapeiaColunas (head peca)

verificaAtribuicaoPecaRecursiva :: [Int] -> [Int] -> [Int] -> Bool  
verificaAtribuicaoPecaRecursiva [] _ [] = True
verificaAtribuicaoPecaRecursiva (x:xs) linha (a:as) = ((linha !! x) == 0 && a /= 0) && verificaAtribuicaoPecaRecursiva xs linha as

mapeiaColunas :: [Int] -> [Int]
mapeiaColunas formatoPeca = [menorIndice..maiorIndice]
    where
        tamanhoPeca = length formatoPeca
        menorIndice = if even tamanhoPeca then 5 - (tamanhoPeca `div` 2) else 5 - (tamanhoPeca `div` 2) - 1
        maiorIndice = 5 + (tamanhoPeca `div` 2) - 1

geraPeca :: Int -> Peca
geraPeca indice = Peca {qualPeca = indice, formatoPeca = funcaoGeradora !! indice}
    where
        funcaoGeradora = [geraI, geraL, geraO, geraR, geraS, geraT, geraZ]

geraI :: [[Int]]
geraI = [
    [1, 1, 1, 1]
    ]

geraL :: [[Int]]
geraL = [
    [2, 2, 2],
    [2, 0, 0]
    ]

geraO :: [[Int]]
geraO = [
    [3, 3],
    [3, 3]
    ]

geraR :: [[Int]]
geraR = [
    [4, 4, 4],
    [0, 0, 4]
    ]

geraS :: [[Int]]
geraS = [
    [5, 5, 0],
    [0, 5, 5]
    ]

geraT :: [[Int]]
geraT = [
    [6, 6, 6],
    [0, 6, 0]
    ]

geraZ :: [[Int]]
geraZ = [
    [0, 7, 7],
    [7, 7, 0]
    ]