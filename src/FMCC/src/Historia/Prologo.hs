module Historia.Prologo(
    comecoHistoria
) where
import Historia
import Util.Lib
import Historia.Fase1

comecoHistoria::IO()
comecoHistoria = do
    mapM_ printString [inicioHistoria1, inicioHistoria2, inicioHistoria3, inicioHistoria4]
    clearScreen
    putStrLn inicioHistoria5
    escolha <- getLine
    escolhaTreatment escolha 0

escolhaTreatment::String-> Int -> IO()
escolhaTreatment escolha vezes_negado = do

    clearScreen

    case escolha of
        "1" -> do
            setupInicial
        "2" -> jubilado (vezes_negado + 1)
        _ -> do
            putStrLn (textoFormatado("A liberdade de expressão era uma mentira...\n"))
            esperandoEnter
            setupInicial


setupInicial::IO()
setupInicial = do
    mapM_ printString [cursoParte01, cursoParte02, cursoParte03, cursoParte04, cursoParte05, cursoParte06, cursoParte07]
    clearScreen
    atualizaProgresso 1
    escolhaCaminhoCidade

serJubilado :: IO()
serJubilado = do
    putStrLn $ "Heanes: Quantas vezes vou ter que repetir eu não quero fazer isso???\n\n"
     ++ "Carl Wilson: Então você escolheu isso... não me culpe meu amigo\n\n*você foi ejetado para o limbo*\n"
    desbloqueaConquista "Jubilado"
    esperandoEnter

jubilado ::Int -> IO ()
jubilado 8 = serJubilado
jubilado vezes_negado = do
    putStrLn "Pense melhor sobre isso professor...\n"
    putStrLn $ "(1) Sim, quero ajudar.\n" ++ "(2) " ++ concat (replicate vezes_negado "Não. ")
    putStrLn "\n------------------------------------------------------------------------------------\n"
    escolha <- getLine
    escolhaTreatment escolha vezes_negado
