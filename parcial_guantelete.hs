import Text.Show.Functions

type Guantelete = (Material,[Gema])
type Material = String
type Gema = Personaje->Personaje
type Habilidad = String

ironMan = Personaje 10 211 ["pegar"] "iron Man" "polonia"
megaMan = Personaje 100 3 ["golpear"] "mega man" "polonia"
rayoLoco = Personaje 345 12 ["hola man"] "rayito" "askjs"

universo1 = Universo [ironMan,megaMan]
universo2 = Universo [ironMan,megaMan,rayoLoco]

data Personaje = Personaje {
    edad :: Int,
    energia :: Int,
    habilidades :: [Habilidad],
    nombre :: String,
    viveEn :: String
} deriving (Show)

data Universo = Universo {
    habitantes:: [Personaje]
} deriving(Show)

chasquear :: Guantelete -> Universo -> Universo
chasquear unGuantelete unUniverso  | estaCompleto unGuantelete = romperUniverso unUniverso
                                    | otherwise = unUniverso

estaCompleto :: Guantelete -> Bool
estaCompleto unGuantelete = tieneSeisGemas unGuantelete && esDeUru unGuantelete

tieneSeisGemas:: Guantelete -> Bool
tieneSeisGemas  = (>5).length.snd

esDeUru:: Guantelete -> Bool
esDeUru  = (=="Uru").fst

romperUniverso:: Universo -> Universo
romperUniverso unUniverso = unUniverso{habitantes = reducirALaMitad (habitantes unUniverso)}


reducirALaMitad unaLista = take (laMitadDe unaLista) unaLista 
laMitadDe unaLista= div  (length unaLista) 2

paraPendex:: Universo ->Bool
paraPendex unUniverso = any esMenorDe45 (habitantes unUniverso)

esMenorDe45 :: Personaje ->Bool
esMenorDe45  = (<45).edad

suEnergiaTotal :: Universo -> Int
suEnergiaTotal unUniverso = sum (susEnergias unUniverso)

susEnergias unUniverso = map energia (habitantes unUniverso)

--utilizarGuantelete:: Guantelete->Personaje->Personaje
utilizarGuantelete unGuantelete unPersonaje = foldl1 (snd unGuantelete) unPersonaje

laMente:: Personaje->Int->Personaje
laMente unPersonaje numero= restarEnergia numero unPersonaje 
 
restarEnergia numero personaje = personaje{energia= (energia personaje) - numero}

elAlma :: Personaje->Habilidad->Personaje
elAlma unPersonaje unaHabilidad = (quitarHabilidad unaHabilidad).(restarEnergia 10) $ unPersonaje

quitarHabilidad unaHabilidad unPersonaje = unPersonaje{habilidades = filter (==unaHabilidad) (habilidades unPersonaje)}

elEspacio :: Personaje->String->Personaje
elEspacio unPersonaje unPlaneta = (restarEnergia 20).ahoraViveEn unPlaneta $ unPersonaje
    
    
ahoraViveEn  unPlaneta unPersonaje = unPersonaje{viveEn = unPlaneta}

elPoder :: Personaje->Personaje
elPoder unPersonaje = analizarSusHabilidades.(restarEnergia (energia unPersonaje)) $ unPersonaje

analizarSusHabilidades :: Personaje->Personaje
analizarSusHabilidades unPersonaje 
    | tieneDosOMenosHabilidades unPersonaje = quitarTodasSusHabilidades unPersonaje
    | otherwise = unPersonaje

tieneDosOMenosHabilidades  = (<3).length.habilidades 
quitarTodasSusHabilidades :: Personaje->Personaje
quitarTodasSusHabilidades unPersonaje = unPersonaje{habilidades= []}

elTiempo :: Personaje->Personaje
elTiempo  = (restarEnergia 50).reducirEdad 

reducirEdad unPersonaje= unPersonaje{edad= ((max 18).(div (edad unPersonaje) 2))}

