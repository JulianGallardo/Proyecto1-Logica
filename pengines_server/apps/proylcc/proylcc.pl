:- module(proylcc, 
	[  
		join/4
	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

join(Grid, NumOfColumns, Path, RGrids):- calcularSuma(Grid, NumOfColumns, Path, Suma), calcularProximaPotencia(Suma, 2, Res), generarLista(Grid, NumOfColumns, Path, GridA, Res), RGrids = [GridA].

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

bajarColumnas(Grid,[X],NumOfColumns,GridRes, Posicion):- X = 0, Posicion is 10, PosicionAux is Posicion-5, buscarElementoI(Grid, PosicionAux, Res), reemplazarElementoI(Grid, Posicion, Res, GridRes).
bajarColumnas(Grid,[X],NumOfColumns,Grid, Posicion):- X\=0, Posicion is 10.
bajarColumnas(Grid,[X|Xs],NumOfColumns,GridRes, Posicion):-bajarColumnas(Grid, Xs,NumOfColumns,GridAux, PosicionAux), Posicion is PosicionAux-1, X = 0, Posicion2 is Posicion-5, Posicion2>=0, buscarElementoI(Grid, Posicion2, Res), reemplazarElementoI(GridAux, Posicion, Res, GridRes).
bajarColumnas(Grid,[X|Xs],NumOfColumns,GridRes, Posicion):-bajarColumnas(Grid, Xs,NumOfColumns,GridAux, PosicionAux), Posicion is PosicionAux-1, X = 0, Posicion2 is Posicion-5, Posicion2<0, generarPotencia2Random(Res), reemplazarElementoI(GridAux, Posicion, Res, GridRes).
bajarColumnas(Grid,[X|Xs],NumOfColumns,GridAux, Posicion):-bajarColumnas(Grid, Xs,NumOfColumns,GridAux, PosicionAux), X \= 0. 

generarPotencia2Random(Potencia) :- random(Rand), Exponente is floor(log(Rand)/log(2)), Potencia is 2^Exponente.

poner todos los 0 en una lista de posiciones
por cada posicion de la lista hacer un repeat until haya un numero valido que sacar de arriba

