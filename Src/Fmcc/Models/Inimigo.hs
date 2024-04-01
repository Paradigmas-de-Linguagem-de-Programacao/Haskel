module Models.Inimigo(
    Inimigo(..),
    getNomeInimigo,
    toStringInimigo
) where 
    
    data Inimigo = Inimigo {
        nome::String,
        ataque::Int,
        defesa::Int,
        vida::Int,
        habilidadeEspecial::Int
}   deriving (Show, Read)

    getNomeInimigo:: Inimigo -> String
    getNomeInimigo = nome

    toStringInimigo:: Inimigo -> String
    toStringInimigo inimigo = "Nome: " ++ nome inimigo ++ "\nVida: " ++ show (vida inimigo) ++ "\nAtaque: " ++ show (ataque inimigo) ++",   Defesa: " ++ show (defesa inimigo)