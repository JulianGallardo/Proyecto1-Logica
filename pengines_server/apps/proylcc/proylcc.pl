:- module(proylcc, 
	[  
		join/4
	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

join(Grid, NumOfColumns, Path, RGrids):- calcularSuma(Grid, NumOfColumns, Path, Suma), calcularProximaPotencia(Suma, 2, Res), generarLista(Grid, NumOfColumns, Path, GridA, Res),
	gravedad(GridA,GridB),reemplazarPorPotencias(GridB,GridC), RGrids = [GridA,GridB].

sacarIDelPath([X], _NumOfColumns, X).
sacarIDelPath([X|Xs], NumOfColumns, Res1) :- sacarIDelPath(Xs, NumOfColumns, ResAux), Res1 is ResAux+(X*NumOfColumns).

borrarElementoI([_X|Xs], 0, [0|Xs]).
borrarElementoI([X|Xs], Y, [X|Res]):- YAux is Y-1, borrarElementoI(Xs, YAux, Res).

reemplazarElementoI([_X|Xs], 0, Z, [Z|Xs]).
reemplazarElementoI([X|Xs], Y, Z, [X|Res]):- YAux is Y-1, reemplazarElementoI(Xs, YAux, Z, Res).

generarLista(Grid, NumOfColumns, [X], GridRes, Suma) :- sacarIDelPath(X, NumOfColumns, PosI), reemplazarElementoI(Grid, PosI, Suma, GridRes).
generarLista(Grid, NumOfColumns, [X|Xs], GridRes, Suma) :- generarLista(Grid, NumOfColumns, Xs, GridAux, Suma), 
				sacarIDelPath(X, NumOfColumns, PosI), borrarElementoI(GridAux, PosI, GridRes).

				buscarElementoI([X|_XS], 0, X).
			buscarElementoI([_X|Xs], Y, Res):- YAux is Y-1, buscarElementoI(Xs,YAux,Res).			

calcularSuma(Grid, NumOfColumns, [X], Res) :- sacarIDelPath(X, NumOfColumns, PosI), buscarElementoI(Grid, PosI, Res).
calcularSuma(Grid, NumOfColumns, [X|Xs], Res) :- calcularSuma(Grid, NumOfColumns, Xs, ResAux),
	sacarIDelPath(X, NumOfColumns, PosI), buscarElementoI(Grid, PosI, ResAux2), Res is ResAux+ResAux2.

calcularProximaPotencia(X, Y, Res) :- X>(2**Y), Y2 is Y+1, calcularProximaPotencia(X, Y2, Res).
calcularProximaPotencia(X, Y, Res) :- X=<(2**Y), Res is 2**Y.


/**
bajarColumnas(Grid,[X],NumOfColumns,GridRes, Posicion):- X = 0, Posicion is 10, PosicionAux is Posicion-5, buscarElementoI(Grid, PosicionAux, Res), reemplazarElementoI(Grid, Posicion, Res, GridRes).
bajarColumnas(Grid,[X],NumOfColumns,Grid, Posicion):- X\=0, Posicion is 10.
bajarColumnas(Grid,[X|Xs],NumOfColumns,GridRes, Posicion):-bajarColumnas(Grid, Xs,NumOfColumns,GridAux, PosicionAux), Posicion is PosicionAux-1, X = 0, Posicion2 is Posicion-5, Posicion2>=0, buscarElementoI(Grid, Posicion2, Res), reemplazarElementoI(GridAux, Posicion, Res, GridRes).
bajarColumnas(Grid,[X|Xs],NumOfColumns,GridRes, Posicion):-bajarColumnas(Grid, Xs,NumOfColumns,GridAux, PosicionAux), Posicion is PosicionAux-1, X = 0, Posicion2 is Posicion-5, Posicion2<0, generarPotencia2Random(Res), reemplazarElementoI(GridAux, Posicion, Res, GridRes).
bajarColumnas(Grid,[X|Xs],NumOfColumns,GridAux, Posicion):-bajarColumnas(Grid, Xs,NumOfColumns,GridAux, PosicionAux), X \= 0. 

**/

generarPotencia2Random(Potencia) :- random(Rand), Exponente is floor(log(Rand)/log(2)), Potencia is 2^Exponente.

sacarIColumna([X], X).
sacarIColumna([X|Xs], Res) :- sacarIColumna(Xs, Res).

sacarColumnaI([_X], Contador, Numero, NumMod, Res) :- Numero =\= Contador mod NumMod, Res = [].
sacarColumnaI([X], Contador, Numero, NumMod, Res) :- Numero =:= Contador mod NumMod, Res = [X].
sacarColumnaI([X|Xs], Contador, Numero, NumMod, [X|ResAux]) :-  Numero =:= Contador mod NumMod, Contador2 is Contador+1, sacarColumnaI(Xs, Contador2, Numero, NumMod, ResAux).
sacarColumnaI([_|Xs], Contador, Numero, NumMod, Res) :-  Numero =\= Contador mod NumMod, Contador2 is Contador+1, sacarColumnaI(Xs, Contador2, Numero, NumMod, Res).

sacarDistintos0([X], [X]) :- X =\= 0.
sacarDistintos0([X], []) :- X =:= 0.
sacarDistintos0([X|Xs], [X|Res]) :- X =\= 0, sacarDistintos0(Xs, Res).
sacarDistintos0([X|Xs], Res) :- X =:= 0, sacarDistintos0(Xs,Res).

poner0Adelante(Lista,0,Lista).
poner0Adelante(Lista,1,[0|Lista]).
poner0Adelante(Lista,Cantidad,[0|Res]):-CantidadAux is Cantidad-1,poner0Adelante(Lista,CantidadAux,Res).  

ordenarColumnas(Grid, 4, Res) :- sacarColumnaI(Grid, 0, 4, 5, ResAux1), sacarDistintos0(ResAux1, ResAux2), length(ResAux1,CantElementos1),
	length(ResAux2,CantElementos2), Cantidad0 is CantElementos1-CantElementos2, poner0Adelante(ResAux2, Cantidad0, Res).
ordenarColumnas(Grid, Contador, Res) :- ContadorAux is Contador+1,ordenarColumnas(Grid,ContadorAux,ColumnasOrdenadas),sacarColumnaI(Grid, 0, Contador,5, ResAux1), sacarDistintos0(ResAux1, ResAux2), length(ResAux1,CantElementos1),
	length(ResAux2,CantElementos2), Cantidad0 is CantElementos1-CantElementos2, poner0Adelante(ResAux2, Cantidad0, ResAux3),append(ResAux3,ColumnasOrdenadas,Res).

ponerColumnasBien(Grid, 4, Res):-sacarColumnaI(Grid,0,4,8,Res).
ponerColumnasBien(Grid, Contador, Res) :- ContadorAux is Contador+1,ponerColumnasBien(Grid,ContadorAux,ResAux2),
	sacarColumnaI(Grid, 0, Contador,8, ResAux1), append(ResAux1, ResAux2, Res).

gravedad(Grid, Res) :- ordenarColumnas(Grid, 0, ResAux), ponerColumnasBien(ResAux, 0, Res).

reemplazarPorPotencias([X], [X]) :- X\=0.
reemplazarPorPotencias([X], [Res]) :- X=0, generarPotencia2Random(Res).
reemplazarPorPotencias([X|Xs], [X|Res]) :- X\=0, reemplazarPorPotencias(Xs, Res).
reemplazarPorPotencias([X|Xs], [Y|Res]) :- X=0, generarPotencia2Random(Y), reemplazarPorPotencias(Xs, Res).

