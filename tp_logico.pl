comio(pumba, vaquitaSanAntonio(gervasia,3)).
comio(pumba, hormiga(federica)).
comio(pumba, hormiga(tuNoEresLaReina)).
comio(pumba, cucaracha(ginger,15,6)).
comio(pumba, cucaracha(erikElRojo,25,70)).

comio(timon, vaquitaSanAntonio(romualda,4)).
comio(timon, cucaracha(gimeno,12,8)).
comio(timon, cucaracha(cucurucha,12,5)).

comio(simba, vaquitaSanAntonio(remeditos,4)).
comio(simba, hormiga(schwartzenegger)).
comio(simba, hormiga(niato)).
comio(simba, hormiga(lula)).

comio(shenzi,hormiga(conCaraDeSimba)).

pesoHormiga(2). 

%peso(Personaje, Peso) 
peso(pumba, 100). 
peso(timon, 50). 
peso(simba, 200). 
peso(scar, 300).
peso(shenzi, 400).
peso(banzai, 500).
peso(vaquitaSanAntonio(_,Peso),Peso).
peso(cucaracha(_,_,Peso),Peso).
peso(hormiga(_),P):- pesoHormiga(P).


persigue(scar, timon).
persigue(scar, pumba).
persigue(shenzi, simba).
persigue(shenzi, scar).
persigue(banzai, timon).

%Ejercicio 1a
jugosita(cucaracha(_,Tamanio,Peso)):- comio(_,cucaracha(_,Tamanio,Otropeso)), Peso > Otropeso. 
%Ejercicio 1b
hormigofilico(Personaje):-	peso(Personaje,_),findall(Hormiga,comio(Personaje,hormiga(Hormiga)),Hormigas),length(Hormigas,Cant),
Cant >= 2 .
%Ejercicio 1c
cucarachofobico(Personaje):-  peso(Personaje,_), not(comio(Personaje,cucaracha(_,_,_))).	
%Ejercicio 1d
picarones(Picarones):- findall(Personaje, picaron(Personaje), Picarones). 

picaron(Personaje):- comio(Personaje,Cucaracha), jugosita(Cucaracha).
picaron(Personaje):- comio(Personaje,vaquitaSanAntonio(remeditos,_)).
picaron(pumba).

%Ejercicio 2a
cuantoEngorda(Personaje,PesoTotal) :-  peso(Personaje,_), not(comio(_,Personaje)),not(persigue(Personaje,_)), findall(Peso, engordaPersonaje(Personaje,Peso),Listapesos), sumlist(Listapesos,PesoTotal).

engordaPersonaje(Personaje, Peso):- comio(Personaje, Victima), peso(Victima,Peso).

%Ejercicio 2b
cuantoEngorda2(Personaje,PesoTotal) :-  peso(Personaje,_), not(comio(_,Personaje)), findall(Peso, engordaPersonaje2(Personaje,Peso),Listapesos), sumlist(Listapesos,PesoTotal).

engordaPersonaje2(Personaje, Peso):- personajecome(Personaje, Victima), peso(Victima,Peso).
    
personajecome(Personaje,Victima):- comio(Personaje,Victima).
personajecome(Personaje,Victima):- persigue(Personaje,Victima).
%Ejercicio 2c
cuantoEngorda3(Personaje,PesoTotal) :-  peso(Personaje,_), not(comio(_,Personaje)), findall(Peso, engordaPersonaje3(Personaje,Peso),Listapesos), sumlist(Listapesos,PesoTotal).
cuantoEngorda3(Personaje, 0):- comio(_,Personaje).

engordaPersonaje3(Personaje, Peso):- personajecome(Personaje, Victima), peso(Victima,Peso1), cuantoEngorda3(Victima,Peso2), Peso is Peso1 + Peso2.
