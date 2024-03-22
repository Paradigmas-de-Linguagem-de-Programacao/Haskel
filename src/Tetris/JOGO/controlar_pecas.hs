{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Data.Bits (Bits(shift))
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use foldr" #-}

congelarTudo :: [[Int]] -> [[Int]]
congelarTudo [] = []
congelarTudo (linha: rabo) = congelarLinha linha : congelarTudo rabo

congelarLinha :: [Int] -> [Int]
congelarLinha [] = []
congelarLinha (0 : rabo) = 0 : congelarLinha rabo
congelarLinha (numero : rabo)
    | numero < 10 = (numero + 10) : congelarLinha rabo
    | otherwise = numero : congelarLinha rabo

verificaShiftDireitaLinha :: [Int] -> Bool
verificaShiftDireitaLinha [] = True
verificaShiftDireitaLinha [c] = c == 0
verificaShiftDireitaLinha (c : m : r)
    | cEhPeca && mEhCongelado = False
    | otherwise = verificaShiftDireitaLinha (m : r)
    where
        cEhPeca = c > 0 && c < 10
        mEhCongelado = m > 10

verificaShiftDireita :: [[Int]] -> Bool
verificaShiftDireita [] = True
verificaShiftDireita (c : r) = verificaShiftDireitaLinha c && verificaShiftDireita r

verificaShiftEsquerdaLinhaAuxiliar :: [Int] -> Bool
verificaShiftEsquerdaLinhaAuxiliar [] = True
verificaShiftEsquerdaLinhaAuxiliar [m] = True
verificaShiftEsquerdaLinhaAuxiliar (c: m : r)
    | cEhCongelado && mEhPeca = False
    | otherwise = verificaShiftEsquerdaLinhaAuxiliar (m : r)
    where
        cEhCongelado = c > 10
        mEhPeca = m > 0 && m < 10

verificaShiftEsquerdaLinha :: [Int] -> Bool
verificaShiftEsquerdaLinha [] = True
verificaShiftEsquerdaLinha (c : r) = (c > 10 || c == 0) && verificaShiftEsquerdaLinhaAuxiliar (c : r)

verificaShiftEsquerda :: [[Int]] -> Bool
verificaShiftEsquerda [] = True
verificaShiftEsquerda (c : r) = verificaShiftEsquerdaLinha c && verificaShiftEsquerda r

shiftEsquerdaLinha :: [Int] -> [Int]
shiftEsquerdaLinha [] = []
shiftEsquerdaLinha [c] = [c]
shiftEsquerdaLinha (c : m : r)
    | mEhPeca = m : shiftEsquerdaLinha (c : r)
    | otherwise = c : shiftEsquerdaLinha (m : r)
    where
        mEhPeca = m > 0 && m < 10

shiftEsquerda :: [[Int]] -> [[Int]]
shiftEsquerda [] = []
shiftEsquerda (c : r) = shiftEsquerdaLinha c : shiftEsquerda r

trocaDireita :: [Int] -> [Int]
trocaDireita [] = []
trocaDireita [c] = [c]
trocaDireita (c: m : r)
    | cEhPeca = m : c : r
    | otherwise = c : m : r
    where cEhPeca = c > 0 && c < 10

shiftDireitaLinha :: [Int] -> [Int]
shiftDireitaLinha [] = []
shiftDireitaLinha [c] = [c]
shiftDireitaLinha (c : m : r) = trocaDireita (c : shiftDireitaLinha (m : r) )

shiftDireita :: [[Int]] -> [[Int]]
shiftDireita [] = []
shiftDireita (c : r) = shiftDireitaLinha c : shiftDireita r

verificarShiftBaixo :: [[Int]] -> Bool
verificarShiftBaixo [] = False
verificarShiftBaixo [c] = True
verificarShiftBaixo (c : m : r) = verificarShiftBaixoLinha m c && verificarShiftBaixo (m : r)

verificarShiftBaixoAuxiliar :: [[Int]] -> Bool
verificarShiftBaixoAuxiliar [] = False
verificarShiftBaixoAuxiliar [c] = False
verificarShiftBaixoAuxiliar (c : m : r) = verificarShiftBaixoLinhaAuxiliar m c && verificarShiftBaixo (m : r)

verificarShiftBaixoLinhaAuxiliar :: [Int] -> [Int] -> Bool
verificarShiftBaixoLinhaAuxiliar [] [] = True
verificarShiftBaixoLinhaAuxiliar _ [] = False
verificarShiftBaixoLinhaAuxiliar [] _ = False
verificarShiftBaixoLinhaAuxiliar (c1:r1) (c2:r2)
    | c1EhPeca && c2Proibe = False
    | otherwise = verificarShiftBaixoLinhaAuxiliar r1 r2
    where
        c1EhPeca = c1 > 0 && c1 < 10
        c2Proibe = c2 > 0

verificarShiftBaixoLinha :: [Int] -> [Int] -> Bool
verificarShiftBaixoLinha [] [] = True
verificarShiftBaixoLinha _ [] = False
verificarShiftBaixoLinha [] _ = False
verificarShiftBaixoLinha (c1:r1) (c2:r2)
    | c1EhPeca && c2EhCongelado = False
    | otherwise = verificarShiftBaixoLinha r1 r2
    where
        c1EhPeca = c1 > 0 && c1 < 10
        c2EhCongelado = c2 > 10

shiftBaixoLinha :: [Int] -> [Int] -> ([Int], [Int])
shiftBaixoLinha [] [] = ([], [])
shiftBaixoLinha [] ys = ([], ys)
shiftBaixoLinha xs [] = (xs, [])
shiftBaixoLinha (c1:r1) (c2:r2)
    | c1EhCongelado || c2EhCongelado = (c1:r1', c2:r2')
    | otherwise = (c2:r1', c1:r2')
    where
        c1EhCongelado = c1 > 10
        c2EhCongelado = c2 > 10
        (r1', r2') = shiftBaixoLinha r1 r2