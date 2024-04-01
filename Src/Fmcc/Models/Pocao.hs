module Models.Pocao where
    import Data.List (intercalate)

    data Pocao = Pocao {
        nome::String,
        vida::Int,
        preco::Int,
        defesa::Int,
        ataque::Int,
        quantidade::Int
    } deriving (Show, Read)

    getNomePocao::Pocao -> String
    getNomePocao =  nome

    getPrecoPocao:: Pocao -> Int
    getPrecoPocao = preco

    getQuantidadePocao:: Pocao -> Int
    getQuantidadePocao = quantidade

    pocaoPlayer :: [Pocao] -> String
    pocaoPlayer pocoes = intercalate ",  " (map toStringPocao pocoes)

    toStringPocao::Pocao -> String
    toStringPocao pocao = "Nome: " ++ nome pocao ++ "  , Quantidade: " ++ show (quantidade pocao)


    toStringPocaoLoja :: [Pocao] -> String
    toStringPocaoLoja [] = "\n"
    toStringPocaoLoja pocoes = intercalate "\n" (map pocaoLoja pocoes)

    pocaoLoja :: Pocao -> String
    pocaoLoja pocao = "Nome: " ++nome pocao ++ ", Preco: " ++ show (preco pocao)

