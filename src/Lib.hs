module Lib where
import Text.Show.Functions

laVerdad = True


-- Punto 1)
type Gema = Personaje -> Personaje

data Guantelete = UnGuantelete {
    material :: String,
    gemas :: [Gema]
}deriving (Show)

data Personaje = UnPersonaje {
    edad :: Float,
    energia :: Float,
    habilidades :: [String],
    nombre :: String,
    planeta :: String
}deriving (Show)

type Universo = [Personaje]


guanteleteCompleto :: Guantelete -> Bool
guanteleteCompleto guantelete = ((==6).length.gemas) guantelete && material guantelete == "uru"

chasquear :: Guantelete -> Universo -> Universo
chasquear guantelete universo | guanteleteCompleto guantelete = reducirMitad universo
                              | otherwise = universo

reducirMitad :: Universo -> Universo
reducirMitad universo = take (length universo `div` 2) universo