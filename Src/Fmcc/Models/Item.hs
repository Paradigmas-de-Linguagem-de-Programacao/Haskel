module Models.Item where
    import Data.List (intercalate)

    data Item = Item {
        nome::String,
        preco::Int,
        ataque::Int,
        defesa::Int,
        descricao::String
    } deriving (Show, Read, Eq)

    getPrecoItem :: Item -> Int
    getPrecoItem = preco

    getNomeItem :: Item -> String
    getNomeItem = nome

    toStringItensLoja :: [Item] -> String
    toStringItensLoja [] = "\n"
    toStringItensLoja itens = intercalate "\n----------------------\n" (map itemLoja itens)

    itemLoja :: Item -> String
    itemLoja item = "Nome: " ++ (nome item) ++ "\nAtaque: " ++ show (ataque item) ++ ",  Defesa: " ++ show (defesa item) ++ "\nPreco: " ++ show (preco item)

    itensPlayer :: [Item] -> String
    itensPlayer [] = "\n"
    itensPlayer itens = intercalate "\n----------------------\n" (map itemP itens)

    itemP:: Item -> String
    itemP item = "Nome: " ++ (nome item) ++ "\nAtaque: " ++ show (ataque item) ++ ",  Defesa: " ++ show (defesa item)
