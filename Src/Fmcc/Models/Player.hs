module Models.Player where
    import Models.Pocao hiding (nome, vida, ataque, defesa)
    import Models.Item hiding (nome, ataque, defesa)

    data Player = Player {
        nome::String,
        vida::Int,
        gold::Int,
        defesa::Int,
        ataque::Int,
        equipamentos::[Item],
        pocoes::[Pocao],
        progresso::Int,
        pocoesTomadas::Int
    } deriving (Show, Read)


    getProgresso :: Player -> Int
    getProgresso = progresso

    attProgresso:: Player -> Int -> Player
    attProgresso heanes novoProgresso = heanes {progresso = novoProgresso}

    getGold:: Player -> Int
    getGold = gold

    modificaGold:: Player -> Int -> Player
    modificaGold heanes goldExtra = heanes{gold = getGold heanes + goldExtra}

    getVida:: Player -> Int
    getVida = vida

    reset :: Player -> Player
    reset heanes = Player {
        nome = "Heanes",
        vida = 100,
        gold = 0,
        defesa = 5,
        ataque = 5,
        equipamentos = equipamentos heanes,
        pocoes = pocoes heanes,
        progresso = progresso heanes,
        pocoesTomadas = 0
        }

    toString :: Player -> String
    toString heanes = "Vida: " ++ show (vida heanes) ++ "/100\n" ++ "Ataque: "++ show (ataque heanes) ++",   Defesa: " ++ show (defesa heanes) ++ "\nPocoes\n" ++ pocaoPlayer (pocoes heanes)


