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
estaEnPeligro linea = (poderAtaquePlantas (plantas linea))<(mordiscosZombies(zombies linea))||(esLineaPeligrosa linea)
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
mismaEspecialidad [] = []
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
resultadoAtaqueAPlanta:: Planta->Zombie->Planta--Estaba mal definida la función
resultadoAtaqueAPlanta planta zombie =  Planta (especie planta) (restarPuntosDeVida planta zombie) (cantSoles planta) (poderAtaque planta) 

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

--Ejercicio 3, hago esta parte porque la necesito para el ejercicio 7
data Horda= Horda{
  parHorda:: [([Zombie],LineaDeDefensa)]
}deriving (Show, Eq)

septimo_regimiento :: Horda
septimo_regimiento = Horda [([zombieBalloon,zombieBalloon],linea1),([zombieNewspaper],linea2),([zombieBalloon,zombieBalloon],linea3)]

region :: Horda
region = Horda [([gargantuar,gargantuar],linea1),([gargantuar,gargantuar],linea2),([gargantuar,gargantuar],linea3)]

data Jardin=UnJardin{
  lineasJardin :: [LineaDeDefensa]
}deriving (Show, Eq)

jardin::Jardin
jardin = UnJardin [linea1,linea2,linea3] 

agregar:: Jardin->Horda->Jardin
agregar jardin horda =  UnJardin (map (\tupla->LineaDeDefensa (plantas (snd tupla)) ((fst tupla)++zombies (snd tupla))) (parHorda horda))

--4)
rondaDeAtaque:: Planta->Zombie->(Planta,Zombie)
rondaDeAtaque planta zombie= (resultadoAtaqueAPlanta planta zombie,resultadoAtaqueAZombie planta zombie)

--Version recursiva en caso de que se enfrenten más de una vez (en el ejemplo hay un 3 en la consulta? asumí que es por eso. Lo dejo como está arriba)
--rondaDeAtaque:: Planta->Zombie->Number->(Planta,Zombie)
--rondaDeAtaque planta zombie numeroRonda
--                             |(numeroRonda==0) = (planta,zombie)
--                              |otherwise =  rondaDeAtaque (resultadoAtaqueAPlanta planta zombie) (resultadoAtaqueAZombie planta zombie) (numeroRonda - 1)

--5)
murioUnaPlanta:: Planta->Zombie->Bool
murioUnaPlanta planta zombie = (puntosDeVida (resultadoAtaqueAPlanta planta zombie)<=0) ==True

murioUnZombie:: Planta->Zombie->Bool
murioUnZombie planta zombie = (nivelDeMuerte (resultadoAtaqueAZombie planta zombie)<=0) ==True

verificarSi:: (Planta->Zombie->Bool)->Planta->Zombie->Bool
verificarSi criterio planta zombie = criterio planta zombie

-- Ejercicio 6
ataqueSistematico :: [Planta] -> Zombie -> [Planta]
ataqueSistematico plantasAtacadas zombie = [resultadoAtaqueAPlanta p zombie|p<-plantasAtacadas]
-- Ejercicio 7, esta incompleto, cuando estén los ej 4 y 5 lo termino porque se necesita saber si un zombie o una planta murio

zombiesDeHorda :: Horda->[Zombie]
zombiesDeHorda horda = concatMap fst (parHorda horda)

noEsPeligroso :: Zombie->Bool
noEsPeligroso zombie = not (esPeligroso zombie)

obtenerZombiesNoPeligrosos :: Horda->[Zombie]
obtenerZombiesNoPeligrosos horda = filter (noEsPeligroso) (zombiesDeHorda horda)

ataqueSistematico2 :: [Planta] -> [Zombie] -> [Planta]
ataqueSistematico2 plantasAtacadas listazombies = [resultadoAtaqueAPlanta p z |p<-plantasAtacadas, z<-listazombies]

resultadoDeAtaque :: LineaDeDefensa->Horda-> LineaDeDefensa
resultadoDeAtaque linea horda = LineaDeDefensa (ataqueSistematico2 (plantas linea)(zombiesDeHorda horda))  (zombies linea ++ zombiesDeHorda horda)

-- Ejercicio 8, lo mismo no se puede hacer hasta que no esten 4 y 5
obtenerListasPlantas::  Jardin-> Horda->[Planta]--Trate de hacerla de orden superior pero no compilaba, hice dos listas distintas
obtenerListasPlantas jardin horda = concat (map (plantas) (lineasJardin (agregar jardin horda)))

obtenerListasZombies::  Jardin-> Horda->[Zombie]
obtenerListasZombies jardin horda = concat (map (zombies) (lineasJardin (agregar jardin horda)))

peleaPlantasYZombies:: [Planta]->[Zombie]->[(Planta,Zombie)]
peleaPlantasYZombies plantas zombies = zipWith rondaDeAtaque plantas zombies

theZombiesAteYourBrains:: Jardin->Horda->Bool
theZombiesAteYourBrains jardin horda= any (\(planta, zombie) -> murioUnaPlanta planta zombie) (peleaPlantasYZombies (obtenerListasPlantas jardin horda) (obtenerListasZombies jardin horda))


--Ejercicio 9
tieneMenosLetras :: Zombie->LineaDeDefensa->Bool
tieneMenosLetras zombie linea = notElem False ((compararLetras.zombies)linea zombie)

compararLetras :: [Zombie]->Zombie->[Bool]
compararLetras = (\listazombies zombie  -> [(length.nombre)zombie > ((length.nombre).head)listazombies] ++ (compararLetras(tail listazombies)(zombie)))
-- Ejercicio 10 
 -- f ::  Eq a=>a->a->[a]->(a,a)->a
  --f h m p lista
   --              | elem h lista = head (filter (m h) lista)
    --             |otherwise = fst p
-- Es una funcion que no compila 
-- La intencion es que reciba dos elementos y dos listas, en el caso de que el elemento h se encuentre en la lista, 
--devuelve el primer elemento de una nueva lista solo compuesto por aquellos elementos de la lista que sean m o h, es decir devuelve m o h.
-- En el caso de que el elemento h no esté en la lista, devuelve el primer elemento de la tupla p. 
--En primer lugar, hay polimorfismo porque se define la función una vez para luego ser utilizada con cualquier tipo de dato
-- se aplican los conceptos de guardas y está filter que es una función de orden superior, sin embargo no es una funcion expresiva 
--y podría ser mas declarativa(menos detalle algoritmico)
--Funcion mejorada a continuación. 
buscarElementoEnLista :: Eq a=>a->a->[a]->(a,a)->a
buscarElementoEnLista elemento1 elemento2 lista tupla| elemPerteneceALista elemento1 lista = filtrarYDevolverPrimerElemento elemento1 elemento2 lista
                                                     | otherwise = fst tupla

elemPerteneceALista :: Eq a=>a->[a]->Bool
elemPerteneceALista elemento lista = elem elemento lista

filtrarYDevolverPrimerElemento :: Eq a=>a->a->[a]->a
filtrarYDevolverPrimerElemento elemento1 elemento2 lista = head ((filter (==elemento1) lista) ++ (filter (==elemento2) lista))


--Si se trabaja con una lista infinita, el filter recorrerá la lista infinitamente.
-- Ejercicio 11
nivelDeSupervivencia :: LineaDeDefensa->Number
nivelDeSupervivencia = (\linea -> (sumaDeVidaPlantas.plantas)linea - (sumaDeMuerteZombies.zombies)linea)

sumaDeVidaPlantas :: [Planta]->Number
sumaDeVidaPlantas = (\listaplantas -> (puntosDeVida.head)listaplantas + (sumaDeVidaPlantas.tail)listaplantas )

sumaDeMuerteZombies :: [Zombie]->Number
sumaDeMuerteZombies = (\listazombies ->(nivelDeMuerte.head)listazombies + (sumaDeMuerteZombies.tail)listazombies)

