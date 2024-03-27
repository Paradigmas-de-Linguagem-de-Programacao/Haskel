module Util.Estado (
    Estado, EstatisticaPeca, Peca, geraPeca, geraEstadoInicial
) where

data Estado = Estado {
    grid :: [[Int]],
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
    grid = [[0 | _ <- [1..10]] | _ <- [1..20]],
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