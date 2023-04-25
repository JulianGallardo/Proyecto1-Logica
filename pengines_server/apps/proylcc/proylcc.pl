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
