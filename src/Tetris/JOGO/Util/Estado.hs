{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Util.Estado (
    Estado(..), Peca(..), geraPeca, geraEstadoInicial, atribuicaoPeca, verificaAtribuicaoPeca, verificaRotacao, rotacionaPeca, limpaPeca
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
    coordenadas :: ((Int, Int), (Int, Int)),
    cor :: Int
} deriving (Show)

rotacionaPeca :: Estado -> Bool -> Estado
rotacionaPeca estado ehAntiHorario = estado {
    grid = colocarPeca (limpaPeca (grid estado) peca) novaPeca, 
    atualPeca = novaPeca
    }
    where 
        peca = atualPeca estado
        novaPeca = 
            if ehAntiHorario
            then peca {atualEstado = (atualEstado peca + 1) `mod` 4, coordenadas = mapeiaCoordenadasAntiHorario (coordenadas peca)}
            else peca {atualEstado = (atualEstado peca - 1) `mod` 4, coordenadas = mapeiaCoordenadasHorario (coordenadas peca)}

colocarPeca :: [[Int]] -> Peca -> [[Int]]
colocarPeca grid peca = colocarPecaLinhas [x1..x2] [y1,y1-1..y2] grid formatoPeca 
    where
        formatoPeca = formatosPeca peca !! atualEstado peca
        ((x1,y1), (x2,y2)) = coordenadas peca

colocarPecaLinhas :: [Int] -> [Int] -> [[Int]] -> [[Int]] -> [[Int]]
colocarPecaLinhas _ [] grid [] = grid
colocarPecaLinhas colunas (i:is) grid (linha:linhas) = colocarPecaLinhas colunas is novaGrid linhas
    where 
        linhaGrid = grid !! i
        novaGrid = trocaElementoLista grid i (colocaPecaLinha linhaGrid colunas linha)

colocaPecaLinha :: [Int] -> [Int] -> [Int] -> [Int]
colocaPecaLinha linhaGrid [] [] = linhaGrid
colocaPecaLinha linhaGrid (j:js) (elemento: elementos) =
    if elemento == 0
    then colocaPecaLinha linhaGrid js elementos
    else colocaPecaLinha (trocaElementoLista linhaGrid j elemento) js elementos

limpaPeca :: [[Int]] -> Peca -> [[Int]]
limpaPeca grid peca = limpaPecaLinhas grid (cor peca) colunas linhas 
    where 
        ((x1,y1), (x2,y2)) = coordenadas peca
        linhas = [y2..y1]
        colunas = [x1..x2]

limpaPecaLinhas :: [[Int]] -> Int -> [Int] -> [Int] -> [[Int]]
limpaPecaLinhas grid cor colunas [] = grid
limpaPecaLinhas grid cor colunas (i:is) = limpaPecaLinhas novaGrid cor colunas is 
    where
        linhaLimpa = limpaPecaLinha (grid !! i) cor colunas
        novaGrid = trocaElementoLista grid i linhaLimpa 

limpaPecaLinha :: [Int] -> Int -> [Int] -> [Int]
limpaPecaLinha linha cor [] = linha 
limpaPecaLinha linha cor (j:js) = 
    if elemento == cor 
    then limpaPecaLinha (trocaElementoLista linha j 0) cor js 
    else  limpaPecaLinha linha cor js
    where
        elemento = linha !! j

verificaRotacao :: [[Int]] -> Peca -> Bool -> Bool
verificaRotacao grid peca ehAntiHorario = 
    if ehAntiHorario 
    then verificaRotacaoAntihorario grid peca
    else verificaRotacaoHorario grid peca

verificaRotacaoAntihorario :: [[Int]] -> Peca -> Bool
verificaRotacaoAntihorario grid peca = ehCoordenadaValida && podeAtribuir grid (cor peca) [y2'..y1'] [x1'..x2']
    where
        ((x1', y1'), (x2',y2')) = mapeiaCoordenadasAntiHorario (coordenadas peca)
        alturaGrid = length grid
        comprimentoGrid = length (head grid)
        ehCoordenadaValida = x1' > 0 && x2' < comprimentoGrid && y2' > 0 && y1' < alturaGrid

podeAtribuir :: [[Int]] -> Int -> [Int] -> [Int] -> Bool
podeAtribuir grid cor [] colunas = True
podeAtribuir grid cor (i:is) colunas = podeAtribuirLinha (grid !! i) cor colunas && podeAtribuir grid cor is colunas

podeAtribuirLinha :: [Int] -> Int -> [Int] -> Bool
podeAtribuirLinha linhaGrid cor [] = True 
podeAtribuirLinha linhaGrid cor (j:js) = (elemento == 0 || elemento == cor) && podeAtribuirLinha linhaGrid cor js 
    where 
        elemento = linhaGrid !! j

verificaRotacaoHorario :: [[Int]] -> Peca -> Bool
verificaRotacaoHorario grid peca = ehCoordenadaValida && podeAtribuir grid (cor peca) [y2'..y1'] [x1'..x2']
    where
        ((x1', y1'), (x2',y2')) = mapeiaCoordenadasHorario (coordenadas peca)
        alturaGrid = length grid
        comprimentoGrid = length (head grid)
        ehCoordenadaValida = x1' > 0 && x2' < comprimentoGrid && y2' > 0 && y1' < alturaGrid


mapeiaCoordenadasHorario :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
mapeiaCoordenadasHorario ((x1,y1),(x2,y2))
    | dimensaoX == dimensaoY = ((x1,y1),(x2,y2))
    | dimensaoX > dimensaoY = ( ( x1, y1+1 ), ( x1 + dimensaoY, (y1+1) - dimensaoX) )
    | dimensaoY > dimensaoX = ( (x1,y1-1), (x1 + dimensaoY , (y1-1) - dimensaoX))
    | otherwise = error "Nem era pra chegar aqui"
    where
        dimensaoX = x2-x1
        dimensaoY = y1-y2

mapeiaCoordenadasAntiHorario :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
mapeiaCoordenadasAntiHorario ( (x1,y1),(x2,y2) ) 
    | dimensaoX == dimensaoY = ((x1,y1),(x2,y2))
    | dimensaoX > dimensaoY = ( ( x2 - dimensaoY, y1+1), ( x2 , (y1+1) - dimensaoX))
    | dimensaoY > dimensaoX = ( ( x2 - dimensaoY, y1-1),( x2 , (y1-1) - dimensaoX) )
    | otherwise = error "Nem era pra chegar aqui"
    where
        dimensaoX = x2-x1
        dimensaoY = y1-y2

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

trocaElementoLista :: [t] -> Int -> t -> [t]
trocaElementoLista [] _ _ = []
trocaElementoLista lista indice elemento = take indice lista ++ [elemento] ++ drop (indice + 1) lista 

atribuicaoPeca :: [[Int]] -> [[Int]] -> [[Int]]
atribuicaoPeca grid peca 
    | tamanhoPeca == 1 = trocaElementoLista grid 19 (atribuicaoPecaRecursiva  indices (last grid) (head peca))
    | otherwise = trocaElementoLista (trocaElementoLista grid 19 (atribuicaoPecaRecursiva  indices (last grid) (head peca))) 18 (atribuicaoPecaRecursiva  indices (grid !! 18)  (last peca))
    where 
        tamanhoPeca = length peca
        indices = mapeiaColunas (head peca)

atribuicaoPecaRecursiva :: [Int] -> [Int] -> [Int] -> [Int]
atribuicaoPecaRecursiva [] grid [] = grid
atribuicaoPecaRecursiva (i:is) grid (a:as) = trocaElementoLista (atribuicaoPecaRecursiva is grid as) i a 

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
geraPeca indice = Peca {qualPeca = indice, formatosPeca = formatos, coordenadas = ((x1,y1), (x2,y2)), atualEstado = 0, cor = indice + 1}
    where
        funcaoGeradora = [geraI, geraL, geraO, geraR, geraS, geraT, geraZ]
        formatos = funcaoGeradora !! indice
        formatoInicial = head formatos
        indices = mapeiaColunas (head formatoInicial)
        alturaFormato = length formatoInicial
        (y1, y2) = (19, 19 - (alturaFormato - 1))
        (x1, x2) = (head indices, last indices)

geraI :: [[[Int]]]
geraI = formatos
    where 
        formatos = [
            [[1, 1, 1, 1]],
            [[1],
             [1],
             [1],
             [1]],
            [[1, 1, 1, 1]],
            [[1],
             [1],
             [1],
             [1]]
            ] 

geraL :: [[[Int]]]
geraL = formatos
    where 
        formatos = [
            [[2, 2, 2],
             [2, 0, 0]],
            [[2, 0],
             [2, 0],
             [2, 2]],
            [[0, 0, 2],
             [2, 2, 2]],
            [[2, 2],
             [0, 2],
             [0, 2]]
            ]

geraO :: [[[Int]]]
geraO = formatos
    where
        formatos = [
            [[3, 3],
             [3, 3]],
            [[3, 3],
             [3, 3]],
            [[3, 3],
             [3, 3]],
            [[3, 3],
             [3, 3]]
            ]

geraR :: [[[Int]]]
geraR = formatos
    where 
        formatos = [
            [[4, 4, 4],
             [0, 0, 4]],
            [[4,4],
             [4,0],
             [4,0]],
            [[4, 0, 0],
             [4, 4, 4]],
            [[0,4],
             [0,4],
             [4,4]]
            ]

geraS :: [[[Int]]]
geraS = formatos
    where formatos = [
            [[5, 5, 0],
             [0, 5, 5]],
            [[0, 5],
             [5, 5],
             [5, 0]],
            [[5, 5, 0],
             [0, 5, 5]],
            [[0, 5],
             [5, 5],
             [5, 0]]
            ]

geraT :: [[[Int]]]
geraT = formatos
    where formatos = [
            [[6, 6, 6],
             [0, 6, 0]],
            [[0, 6],
             [6, 6],
             [0, 6]],
            [[0, 6, 0],
             [6, 6, 6]],
            [[6,0],
             [6,6],
             [6,0]]
            ]

geraZ :: [[[Int]]]
geraZ = formatos
    where formatos = [
            [[0, 7, 7],
             [7, 7, 0]],
            [[7,0],
             [7,7],
             [0,7]],
            [[0, 7, 7],
             [7, 7, 0]],
            [[7,0],
             [7,7],
             [0,7]]
            ]