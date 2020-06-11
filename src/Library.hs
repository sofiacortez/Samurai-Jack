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
thanos :: Personaje 
thanos = UnPersonaje "Aku" 500 [martilloDeThor, escudoCapitanAmerica] 300
bobSponsh :: Personaje
bobSponsh = UnPersonaje "Bob Sponsh" 200 [martilloDeThor, poderQuemador] 800
chinese :: Personaje
chinese = UnPersonaje "Chinese Chinu" 700 [escudoCapitanAmerica, tentaculoArania] 500

-- Elementos
martilloDeThor :: Elemento
martilloDeThor = UnElemento "Maldad" (modificarSalud (-) 400) (modificarSalud (+) 200)
escudoCapitanAmerica :: Elemento
escudoCapitanAmerica = UnElemento "Proteccion" (modificarSalud (-) 50) (modificarSalud (+) 400)
poderQuemador :: Elemento
poderQuemador = UnElemento "Magia" (modificarSalud (-) 600) id
tentaculoArania :: Elemento
tentaculoArania = UnElemento "Maldad" (modificarSalud (-) 200) (modificarSalud (+) 100)
esbirros :: Elemento
esbirros = UnElemento "Maldad" (modificarSalud (-) 1) id
katanaMagica :: Elemento
katanaMagica = UnElemento "Magia" (modificarSalud (-) 1000) (modificarSalud (+) 500)


-- Punto 1
mandarAlAnio :: Number -> Personaje -> Personaje
mandarAlAnio anio personaje = personaje {anioPresente = anio}

modificarSalud :: (Number -> Number -> Number) -> Number -> Personaje -> Personaje
modificarSalud funcion numero personaje = personaje {salud = (max 0.funcion (salud personaje)) (numero)}

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
danioQueProduce personaje elemento =  abs((salud personaje)-(salud.ataque elemento) personaje)

esElementoMortal :: Personaje -> Elemento -> Bool
esElementoMortal personaje elemento = (danioQueProduce personaje elemento) >= (salud personaje)

tieneElementosMortales :: Personaje -> Personaje -> [Bool]
tieneElementosMortales personaje enemigo = map (esElementoMortal personaje) (elementos enemigo)

esEnemigoMortal :: Personaje -> Personaje -> Bool
esEnemigoMortal personaje enemigo = any (== True) (tieneElementosMortales personaje enemigo)

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje = filter (esEnemigoMortal personaje)

-- Punto 3
aplicarVeces :: Number -> (a -> a) -> a -> a
aplicarVeces 0 funcion valor = valor
aplicarVeces cantidadDeVeces funcion valor = foldl (\valor funcion -> funcion valor) valor (replicar cantidadDeVeces funcion)

replicar :: Number -> a -> [a]
replicar 0 _ = []
replicar cantidadDeVeces valor = map (\numero -> valor) [1..cantidadDeVeces]

concentracion :: Number -> Elemento
concentracion nivelConcentracion = UnElemento "Magia" id (aplicarVeces nivelConcentracion meditar)

esbirrosMalvados :: Number -> [Elemento]
esbirrosMalvados cantidad = replicar cantidad esbirros

jack :: Personaje
jack = UnPersonaje "Jack" 300 [concentracion 3, katanaMagica] 200

aku :: Number -> Number -> Personaje
aku salud anioPresente = UnPersonaje "Aku" salud (concentracion 4 : portalAlFuturo anioPresente : esbirrosMalvados (100*anioPresente)) anioPresente

portalAlFuturo :: Number -> Elemento
portalAlFuturo anio = UnElemento "Magia" (mandarAlAnio (anio+2800)) (mandarAlAnio (anio+2800))

-- Punto 4

atacarConElemento :: Personaje -> Elemento -> Personaje
atacarConElemento personaje elemento = (ataque elemento) personaje 

defenderseConElemento :: Personaje -> Elemento -> Personaje
defenderseConElemento personaje elemento = (defensa elemento) personaje

atacar :: Personaje -> Personaje -> Personaje
atacar defensor atacante = foldl (atacarConElemento) defensor (elementos atacante)

defenderse :: Personaje -> Personaje
defenderse atacante = foldl (defenderseConElemento) atacante (elementos atacante)

--estaMuerto :: Personaje -> Bool
--estaMuerto = ((==0).salud)

luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar (UnPersonaje nombre 0 elementos anio) aku = (aku, (UnPersonaje nombre 0 elementos anio))
luchar jack (UnPersonaje nombre 0 elementos anio) = (jack, (UnPersonaje nombre 0 elementos anio))
luchar jack aku = luchar (atacar aku jack) (defenderse jack)

