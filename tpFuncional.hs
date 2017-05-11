import Text.Show.Functions
import Data.List
instance Eq Protagonista where
  protagonista1 == protagonista2 =
    nombre protagonista1 == nombre protagonista2 &&
    vida protagonista1 == vida protagonista2

data Protagonista = Protagonista {
nombre :: Nombre ,
vida :: Vida ,
reaccion :: Reaccion ,
personasQueridas :: [PersonaQuerida]
} deriving(Show)

type Nombre = String
type Vida = Float
type Reaccion = Zombie -> Protagonista -> Protagonista
type PersonaQuerida = Protagonista
type Zombie = Protagonista -> Protagonista

nuevaVida unaVida unProtagonista = unProtagonista { vida = unaVida}
nuevasPersonasQueridas unasPersonasQueridas unProtagonista = unProtagonista { personasQueridas = unasPersonasQueridas}

daryl  = Protagonista "Daryl" 55  (comerSemillasDelErmitaño 30) [carol]
maggie  = Protagonista "Maggie" 100 (caerse ) [carol,krilin]
carol  = Protagonista "Carol" 200 (matarlo ) [victor]
krilin = Protagonista "krilin" 1 (sacrificarse ) []
victor = Protagonista "victor" 11 (sacrificarse ) []

zombieTranqui::Zombie
zombieTranqui victima  = nuevaVida (vida victima - 10) victima
zombieConCasco::Zombie
zombieConCasco victima  = nuevaVida (vida victima /2) victima
zombieSinDientes::Zombie
zombieSinDientes = id
zombieBuenazoAstuto::Int -> Zombie

zombieBuenazoAstuto  hambre victima |((< hambre).cantidadDeAmigos) victima  = zombieSinDientes victima
                                    |otherwise = zombieConCasco victima

zombieReSacado::Zombie
zombieReSacado victima = zombieTranqui (nuevasPersonasQueridas (((map (zombieTranqui.zombieConCasco)).personasQueridas) victima) victima)


matarlo::Reaccion
matarlo zombie protagonista = zombie protagonista
caerse::Reaccion
caerse zombie  protagonista = (zombie.zombie) protagonista
sacrificarse::Reaccion
sacrificarse zombie protagonista = (nuevaVida  0) protagonista


pelearse peleados protagonista = nuevasPersonasQueridas (sacarAmigos peleados protagonista) protagonista
sacarAmigos peleados protagonista = filter (not.(flip elem peleados)) (personasQueridas protagonista)

cantidadDeAmigos  = length.personasQueridas



noQuiere persona = not.(elem persona).personasQueridas

estaMuerto::Protagonista -> Bool
estaMuerto protagonista = ((<= 0).vida) protagonista && all (noQuiere protagonista) [krilin, victor]

morirse = (nuevaVida 0 ).(pelearse [ krilin, victor])

comerSemillasDelErmitaño:: Int -> Reaccion

comerSemillasDelErmitaño cantidadSemillas zombie protagonista | (estaMuerto.zombie) protagonista = zombie protagonista
                                                              | cantidadSemillas <= 10           = nuevaVida 10 protagonista
                                                              | otherwise                        = morirse protagonista

horda victima [] = victima
horda victima (primerZombie:restoDeZombies) | estaMuerto (reaccion victima primerZombie victima) = primerZombie victima
                                            | otherwise = horda victima restoDeZombies



--Pruebas de entregua 1

-- *Main> zombieConCasco carol
-- Protagonista {nombre = "Carol", vida = 100.0, reaccion = <function>, personasQueridas = [Protagonista {nombre = "victor", vida = 11.0, reaccion = <function>, personasQueridas = []}]}

-- *Main> sacrificarse zombieTranqui carol
-- Protagonista {nombre = "Carol", vida = 0.0, reaccion = <function>, personasQueridas = [Protagonista {nombre = "victor", vida = 11.0, reaccion = <function>, personasQueridas = []}]}

-- *Main> caerse zombieConCasco (matarlo zombieSinDientes maggie)
-- Protagonista {nombre = "Maggie", vida = 25.0, reaccion = <function>, personasQueridas =[Protagonista {nombre = "Carol", vida = 200.0, reaccion = <function>, personasQueridas = [Protagonista {nombre = "victor", vida = 11.0, reaccion = <function>, personasQueridas = []}]},Protagonista {nombre = "krilin", vida = 1.0, reaccion = <function>, personasQueridas = []}]}

-- *Main> (zombieTranqui.zombieConCasco) carol
-- Protagonista {nombre = "Carol", vida = 90.0, reaccion = <function>, personasQueridas =[Protagonista {nombre = "victor", vida = 11.0, reaccion = <function>, personasQueridas = []}]}

-- *Main> (caerse zombieSinDientes . matarlo zombieSinDientes) maggie
-- Protagonista {nombre = "Maggie", vida = 100.0, reaccion = <function>, personasQueridas =[Protagonista {nombre = "Carol", vida = 200.0, reaccion = <function>, personasQueridas = [Protagonista {nombre = "victor", vida = 11.0, reaccion = <function>, personasQueridas = []}]},Protagonista {nombre = "krilin", vida = 1.0, reaccion = <function>, personasQueridas = []}]}

-- Pruebas de entrega 2

--  *Main> pelearse [daryl,carol,victor] maggie
--Protagonista {nombre = "Maggie", vida = 100.0, reaccion = <function>, personasQueridas = [Protagonista {nombre = "krilin", vida = 1.0, reaccion = <function>, personasQueridas = []}]}

-- *Main> caerse (zombieBuenazoAstuto 5) maggie
-- Protagonista {nombre = "Maggie", vida = 100.0, reaccion = <function>, personasQueridas = [Protagonista {nombre = "Carol", vida = 200.0, reaccion = <function>, personasQueridas = [Protagonista {nombre = "victor", vida = 11.0, reaccion = <function>, personasQueridas = []}]},Protagonista {nombre = "krilin", vida = 1.0, reaccion = <function>, personasQueridas = []}]}

-- *Main> caerse (zombieBuenazoAstuto 2) maggie
-- Protagonista {nombre = "Maggie", vida = 25.0, reaccion = <function>, personasQueridas = [Protagonista {nombre = "Carol", vida = 200.0, reaccion = <function>, personasQueridas = [Protagonista {nombre = "victor", vida = 11.0, reaccion = <function>, personasQueridas = []}]},Protagonista {nombre = "krilin", vida = 1.0, reaccion = <function>, personasQueridas = []}]}

-- *Main> sacrificarse zombieConCasco maggie
-- Protagonista {nombre = "Maggie", vida = 0.0, reaccion = <function>, personasQueridas = [Protagonista {nombre = "Carol", vida = 200.0, reaccion = <function>, personasQueridas = [Protagonista {nombre = "victor", vida = 11.0, reaccion = <function>, personasQueridas = []}]},Protagonista {nombre = "krilin", vida = 1.0, reaccion = <function>, personasQueridas = []}]}

--horda maggie [zombieTranqui, zombieBuenazoAstuto 20, zombieReSacado, zombieBuenazoAstuto 1, zombieTranqui]
--Protagonista {nombre = "Maggie", vida = 100.0, reaccion = <function>, personasQueridas = [Protagonista {nombre = "Carol", vida = 200.0, reaccion = <function>, personasQueridas = [Protagonista {nombre = "victor", vida = 11.0, reaccion = <function>, personasQueridas = []}]},Protagonista {nombre = "krilin", vida = 1.0, reaccion = <function>, personasQueridas = []}]}

-- *Main> map nombre (filter estaMuerto (personasQueridas (horda maggie [zombieTranqui, zombieBuenazoAstuto 20, zombieReSacado, zombieBuenazoAstuto 1, zombieTranqui])))
-- []
