module Library where
import PdePreludat

data Elemento = UnElemento { tipo :: String,
                             ataque :: (Personaje-> Personaje),
                             defensa :: (Personaje-> Personaje) } deriving (Show)

data Personaje = UnPersonaje { nombre :: String,
                               salud :: Number,
                               elementos :: [Elemento],
                               anioPresente :: Number } deriving (Show)

-- Personajes
aku :: Personaje 
aku = UnPersonaje "Aku" 500 [martilloDeThor, escudoCapitanAmerica] 800

-- Elementos
martilloDeThor :: Elemento
martilloDeThor = UnElemento "Maldad" (modificarSalud (-) 200) (modificarSalud (+) 100)
escudoCapitanAmerica :: Elemento
escudoCapitanAmerica = UnElemento "Proteccion" (modificarSalud (-) 50) (modificarSalud (+) 400)

-- Punto 1
mandarAlAnio :: Number -> Personaje -> Personaje
mandarAlAnio anio personaje = personaje {anioPresente = anio}

modificarSalud :: (Number -> Number -> Number) -> Number -> Personaje -> Personaje
modificarSalud funcion numero personaje = personaje {salud = funcion (salud personaje) numero}

meditar :: Personaje -> Personaje
meditar personaje  = modificarSalud ((+).(/2)) (salud personaje) personaje

causarDanio :: Number -> Personaje -> Personaje
causarDanio = modificarSalud (-)

-- Punto 2
unElementoEsMalvado :: Elemento -> Bool
unElementoEsMalvado elemento = (tipo elemento) == "Malvado"

esMalvado :: Personaje -> Bool
esMalvado personaje = any unElementoEsMalvado (elementos personaje)

danioQueProduce :: Personaje -> Elemento -> Number
danioQueProduce personaje elemento =  abs((salud personaje)(-(salud.ataque elemento) personaje)

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales