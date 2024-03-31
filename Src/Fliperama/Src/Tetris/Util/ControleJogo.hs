{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Src.Fliperama.Src.Tetris.Util.ControleJogo where
import Data.Bits (Bits(shift))

congelarTudo :: [[Int]] -> [[Int]]
congelarTudo = map congelarLinha

congelarLinha :: [Int] -> [Int]
congelarLinha [] = []
congelarLinha (0 : rabo) = 0 : congelarLinha rabo
congelarLinha (numero : rabo)
    | numero < 10 = (numero + 10) : congelarLinha rabo
    | otherwise = numero : congelarLinha rabo

verificaShiftDireitaLinha :: [Int] -> Bool
verificaShiftDireitaLinha [] = True
verificaShiftDireitaLinha [c] = c == 0 || c > 10
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
shiftEsquerda = map shiftEsquerdaLinha

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
shiftDireita = map shiftDireitaLinha 

verificaShiftBaixo :: [[Int]] -> Bool
verificaShiftBaixo [] = False
verificaShiftBaixo (a: as) = verificaShiftBaixoPrimeiraLinha a && verificaShiftBaixoAuxiliar (a:as)

verificaShiftBaixoPrimeiraLinha :: [Int] -> Bool
verificaShiftBaixoPrimeiraLinha [] = True
verificaShiftBaixoPrimeiraLinha (a: as) = (a == 0 || a > 10) && verificaShiftBaixoPrimeiraLinha as

verificaShiftBaixoAuxiliar :: [[Int]] -> Bool
verificaShiftBaixoAuxiliar [] = True
verificaShiftBaixoAuxiliar [c] = False
verificaShiftBaixoAuxiliar [c,r] = verificaShiftBaixoLinha c r
verificaShiftBaixoAuxiliar (c:m:r) = verificaShiftBaixoLinha c m && verificaShiftBaixoAuxiliar (m:r)

verificaShiftBaixoLinha :: [Int] -> [Int] -> Bool
verificaShiftBaixoLinha [] [] = True
verificaShiftBaixoLinha (c1: r1) (c2:r2) = not (c1EhCongelada && c2EhPeca) && verificaShiftBaixoLinha r1 r2
    where
        c1EhCongelada = c1 > 10
        c2EhPeca = c2 > 0 && c2 < 10

shiftBaixo :: [[Int]] -> [[Int]]
shiftBaixo [] = []
shiftBaixo [c] = [c]
shiftBaixo [c,r] = [novoC, novoR]
    where (novoC, novoR) = shiftBaixoLinha (c, r)
shiftBaixo (c:r:m) = novoC : shiftBaixo (novoR : m)
    where (novoC, novoR) = shiftBaixoLinha (c, r)

shiftBaixoLinha :: ([Int], [Int]) -> ([Int], [Int])
shiftBaixoLinha ([], []) = ([], [])
shiftBaixoLinha (a:as, b:bs)
    | bEhPeca = (b:novoAs, a:novoBs)
    | otherwise =  (a:novoAs, b:novoBs)
    where
        bEhPeca = b > 0 && b < 10
        (novoAs, novoBs) = shiftBaixoLinha (as,bs)