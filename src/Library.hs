module Library where
import PdePreludat


doble :: Number -> Number
doble numero = numero + numero

data Campeon = UnCampeon {
    antiguedad :: Number,
    nivel :: Number,
    posicion :: Posicion,
    habilidades :: [String]
} deriving (Show,Eq)

data Posicion = TOP | ADC | SUPP | JG | MID deriving (Show,Eq)

chogath :: Campeon
chogath = UnCampeon 15 8 TOP ["Ruptura", "Grito salvaje", "Clavos vorpalinos", "Festín"]

jinx :: Campeon
jinx = UnCampeon 12 4 ADC ["¡Cambio de armas!", "¡Chispas!", "¡Mascafuegos!", "¡¡Supermegacohete requetemortal!!"]

mel :: Campeon
mel = UnCampeon 0 6 SUPP ["Ráfaga radiante", "Refutar", "Ofuscador solar", "Eclipse dorado"]

yuumi :: Campeon
yuumi = UnCampeon 6 12 SUPP ["Proyectil acechador", "¡Tú y yo!", "Zoomies", "Últimas páginas"]

warwick :: Campeon
warwick = UnCampeon 16 1 JG ["Fauces de la bestia", "Cacería de sangre", "Aullido primigenio", "Opresión infinita"]

equipo :: [Campeon]
equipo = [chogath,jinx,mel,yuumi,warwick]

----

estaRoto :: Campeon -> Bool
estaRoto campeon = antiguedad campeon == 0
estaRoto2 :: Campeon -> Bool
estaRoto2 = (==0).antiguedad

subirDeNivel :: Campeon -> Campeon
subirDeNivel campeon = campeon {nivel = nivel campeon + 1} 

estaPreparado :: [Campeon] -> Bool
estaPreparado = (==5).length

estaEnEarly :: Campeon -> [String]
estaEnEarly campeon | nivel campeon  < 6 = take 3 (habilidades campeon)
                    | otherwise = habilidades campeon



puedeCarrear :: Campeon -> Bool
puedeCarrear campeon = suPosicion campeon || estaRoto campeon

suPosicion :: Campeon -> Bool
suPosicion = tienePosicion.posicion

tienePosicion :: Posicion-> Bool
tienePosicion posicion = elem posicion [TOP,MID,ADC] 

puedeGanar :: [Campeon] -> Bool
puedeGanar = any puedeCarrear 

esInvencible :: [Campeon] -> Bool
esInvencible = all estaRoto2

aptosParaLaTeamfight :: [Campeon] -> [Campeon]
aptosParaLaTeamfight = filter noEnEarly

noEnEarly :: Campeon -> Bool
noEnEarly = (3<).length.estaEnEarly

baron :: [Campeon] -> [Campeon]
baron = map subirDeNivel 



