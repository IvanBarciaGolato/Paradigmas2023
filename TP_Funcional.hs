module Library where
import PdePreludat
doble :: Number-> Number --NO BORRAR, lo utiliza el stack para hacer los tests--
doble numero = numero + numero

--Ejercicio 1:
data Planta = Planta{
  especie:: String,
  puntosDeVida:: Number,
  cantSoles:: Number, 
  poderAtaque:: Number 
} deriving (Show, Eq)

peaShooter = Planta "PeaShooter" 5 0 2
repeater = Planta "Repeater" 5 0 4
sunflower = Planta "Sunflower" 7 1 0
nut = Planta "Nut" 100 0 0
cordyceps = Planta "Cordyceps" 4 8 0
rafflesia = Planta "Rafflesia" 5 0 15

data Zombie = Zombie{
  nombre:: String,
  accesorio:: [String],
  dañoMordida:: Number, 
  nivelDeMuerte:: Number 
}deriving (Show, Eq)

determinarNivelDeMuerte:: String->Number
determinarNivelDeMuerte nombre  = (length.filter(/= ' ')) nombre

zombieBase = Zombie "Zombie Base" [] 1 (determinarNivelDeMuerte (nombre zombieBase))
zombieBalloon = Zombie "Balloon Zombie" ["Globo"] 1 (determinarNivelDeMuerte (nombre zombieBalloon))
zombieNewspaper = Zombie "Newspaper Zombie" ["Diario"] 2 (determinarNivelDeMuerte (nombre zombieNewspaper))
gargantuar = Zombie "Gargantuar Hulk Smash Puny God" ["Poste electrico","Zombie enano"] 30 (determinarNivelDeMuerte (nombre gargantuar))

--Ejercicio 2:
especialidad :: Planta -> String
especialidad planta | (cantSoles planta)>0 = "Proveedora"
                    | (poderAtaque planta) > (puntosDeVida planta) = "Atacante"
                    | otherwise = "Defensiva"

esPeligroso:: Zombie->Bool
esPeligroso zombie = (1<(length.accesorio) zombie) || ((nivelDeMuerte zombie)>10)

--Ejercicio 3:
data LineaDeDefensa = LineaDeDefensa{
  plantas:: [Planta],
  zombies:: [Zombie]
}deriving (Show, Eq)

linea1 = LineaDeDefensa {
  plantas = [sunflower, sunflower, sunflower],
  zombies = []
}

linea2 = LineaDeDefensa {
  plantas = [peaShooter, peaShooter, sunflower, nut],
  zombies = [zombieBase, zombieNewspaper]
}

linea3 = LineaDeDefensa {
  plantas = [sunflower, peaShooter],
  zombies = [gargantuar, zombieBase, zombieBase]
}

convertirEnLista:: Planta->[Planta]
convertirEnLista planta = [planta]

agregarPlanta::Planta->LineaDeDefensa->[Planta]
agregarPlanta planta linea = (plantas linea)++(convertirEnLista planta)

agregarZombie:: Zombie->LineaDeDefensa->[Zombie]
agregarZombie zombie linea = reverse(zombie : (reverse.zombies) linea)

poderAtaquePlantas:: [Planta]->Number
poderAtaquePlantas []=0
poderAtaquePlantas lineaPlantas = poderAtaque((head lineaPlantas))+ poderAtaquePlantas(tail lineaPlantas)

mordiscosZombies:: [Zombie]->Number
mordiscosZombies []=0
mordiscosZombies lineaZombies = dañoMordida((head lineaZombies))+ mordiscosZombies(tail lineaZombies)

listaCantPeligros::[Zombie]->[Bool]
listaCantPeligros []=[]
listaCantPeligros lineaZombies = [(esPeligroso.head) lineaZombies] ++ (listaCantPeligros.tail) lineaZombies

esLineaPeligrosa:: LineaDeDefensa -> Bool
esLineaPeligrosa linea = (notElem False ((listaCantPeligros.zombies) linea))&&(length (zombies linea)>0)
 
estaEnPeligro:: LineaDeDefensa->Bool
estaEnPeligro linea = (poderAtaquePlantas (plantas linea))<(mordiscosZombies(zombies linea))||(esLineaPeligrosa linea)==True
--estaEnPeligro linea = ((poderAtaquePlantas.plantas) linea))<((mordiscosZombies.zombies) linea))||(esLineaPeligrosa linea)
--Me tiraba error en esta línea, lo deje como estaba antes
--IMPORTANTE: Cambiar a como estaba antes una vez finalizado el TP2

esProveedora:: Planta->Bool
esProveedora planta = (especialidad planta =="Proveedora")

listaProveedoras:: [Planta]->[Bool]
listaProveedoras []=[]
listaProveedoras listaPlantas = [(esProveedora.head) listaPlantas] ++ (listaProveedoras.tail) listaPlantas

necesitaSerDefendida:: LineaDeDefensa->Bool
necesitaSerDefendida linea = (notElem False ((listaProveedoras.plantas)linea))

--Ejercicio 4:
long :: [a] -> Number
long [] = 0
long (x:xs) = 1 + long xs

tieneAlMenosDosPlantas :: LineaDeDefensa->Bool
tieneAlMenosDosPlantas linea = (2<=(long.plantas) linea )
obtenerSiguientePlanta :: [Planta]->Planta
obtenerSiguientePlanta listaPlantas = (head.tail) listaPlantas
mismaEspecialidad :: [Planta]->[Bool] 
mismaEspecialidad listaPlantas = [(especialidad.head) listaPlantas == (especialidad.obtenerSiguientePlanta) listaPlantas] ++ (mismaEspecialidad.tail) listaPlantas
lineaMixta :: LineaDeDefensa->Bool
lineaMixta linea = (notElem True ((mismaEspecialidad.plantas) linea)) && tieneAlMenosDosPlantas linea

--Ejercicio 5:
filtrarEspacios :: String -> String
filtrarEspacios cadena = filter (/=' ') cadena 
filtrarEspaciosEnZombie :: Zombie -> String
filtrarEspaciosEnZombie zombie = (filtrarEspacios.nombre) zombie
quitarLetras ::  Planta->Zombie->String
quitarLetras planta zombie = ((drop.poderAtaque) planta) (filtrarEspaciosEnZombie zombie)
resultadoAtaqueAZombie:: Planta->Zombie->Zombie
resultadoAtaqueAZombie planta zombie = Zombie (quitarLetras planta zombie) (accesorio zombie) (dañoMordida zombie) (determinarNivelDeMuerte (quitarLetras planta zombie))


restarPuntosDeVida :: Planta->Zombie->Number
restarPuntosDeVida planta zombie = (puntosDeVida planta) - (dañoMordida zombie)
resultadoAtaqueAPlanta:: Planta->Zombie->Number->Planta--Estaba mal definida la función
resultadoAtaqueAPlanta planta zombie secuencial =  Planta (especie planta) (restarPuntosDeVida planta zombie) (cantSoles planta) (poderAtaque planta) 

--Parte 2 del TP
--Ej 1 Parte 2
--Linea para Ej 1 del TP2
--1 a)
linea4 = LineaDeDefensa {
  plantas = [sunflower, peaShooter],
  zombies = crearListaInfinitaZombies zombieBase
}

crearListaInfinitaZombies:: Zombie->[Zombie]
crearListaInfinitaZombies zombie = (zombie: crearListaInfinitaZombies zombie)

--Al calcular el resultado de la función "estaEnPeligro", nos damos cuenta que la función queda infinitamente
--sumando el mordiscos de todos los zombies base. Podemos decir que la función NO CONVERGE a ningún resultado 
--como tal y nunca termina de generar la cantidad total de mordiscos.

--1 b)
linea5 = LineaDeDefensa {
  plantas = crearListaInfinitaPlantas peaShooter,
  zombies = [gargantuar, zombieBase, zombieBase]
}

crearListaInfinitaPlantas:: Planta->[Planta]
crearListaInfinitaPlantas planta = (planta: crearListaInfinitaPlantas planta)

--Al calcular el resultado de la función "necesitaSerDefendida", nos damos cuenta que la función recorre la lista
--hasta encontrar una planta con una cantidad de soles >0. Como todas las peaShooters siempre van a tener una
--cantidad de soles = 0, se hace evaluación diferida, en donde con solo al analizar la cabeza de la lista, la
--función converge y me devuelve un resultado "False".

linea6 = LineaDeDefensa {
  plantas = crearListaInfinitaPlantas sunflower,
  zombies = [gargantuar, zombieBase, zombieBase]
}
--Al igual que con la función "estaEnPeligro" con los zombies, la función "necesitaSerDefendida"  en este caso, 
--nunca va a devolver a un resultado como tal, ya que la condición es que TODAS las plantas tengan una cantidad 
--de soles >0, como todas las sunflower poseen una cantidad de soles = 1, recorre la lista infinitamente y nunca 
--logra converger a nada.

--2)

cactus= Planta "Cactus" 9 0 0

lineaBalloon = LineaDeDefensa {
  plantas = [cactus],
  zombies = [gargantuar, zombieBalloon, zombieBase, zombieBalloon]
}
--Hacer una función que recorra todos los zombies de la línea y les quite los globos
leQuitaGlobo:: LineaDeDefensa->[Zombie]
leQuitaGlobo lineaDefensa | any (== cactus) (plantas lineaDefensa) =  map (crearListaCompleta) (zombies lineaDefensa)
                          | otherwise = (zombies lineaDefensa)

crearListaCompleta::Zombie->Zombie
crearListaCompleta zombie | (nombre zombie) == "Balloon Zombie" = (Zombie (nombre zombie) [] (dañoMordida zombie) (nivelDeMuerte zombie))
                          | otherwise = (Zombie (nombre zombie) (accesorio zombie) (dañoMordida zombie) (nivelDeMuerte zombie))