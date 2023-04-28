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
 gravedad(GridA,GridB),reemplazarPorPotencias(GridB,GridC), RGrids = [GridA,GridB,GridC].

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

generarPotencia2Random(Resultado) :-
    random(1, 9, Exponente),  % Genera un exponente aleatorio entre 1 y 9
    Resultado is 2 ** Exponente.

sacarIColumna([X], X).
sacarIColumna([_X|Xs], Res) :- sacarIColumna(Xs, Res).

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

ponerColumnasBien(Grid, 8, Res):-sacarColumnaI(Grid,0,8,8,Res).
ponerColumnasBien(Grid, Contador, Res) :- ContadorAux is Contador+1,ponerColumnasBien(Grid,ContadorAux,ResAux2),
 sacarColumnaI(Grid, 0, Contador,8, ResAux1), append(ResAux1, ResAux2, Res).

gravedad(Grid, Res):- ordenarColumnas(Grid,0, ResAux),ponerColumnasBien(ResAux,0, Res).

reemplazarPorPotencias([X], [X]) :- X\=0.
reemplazarPorPotencias([X], [Res]) :- X=0, generarPotencia2Random(Res).
reemplazarPorPotencias([X|Xs], [X|Res]) :- X\=0, reemplazarPorPotencias(Xs, Res).
reemplazarPorPotencias([X|Xs], [Y|Res]) :- X=0, generarPotencia2Random(Y), reemplazarPorPotencias(Xs, Res).


%Empieza_el_booster



filaPos(Pos,0):-Pos<5,Pos>=0.
filaPos(Pos,ResFila):-PosAux is Pos-5,Pos>=0,filaPos(PosAux,ResAux),ResFila is ResAux+1.

calcularFila(Pos1,Fila1,Pos2,Fila2):-filaPos(Pos1,Fila1),filaPos(Pos2,Fila2).

mismaFila(Pos1,Pos2):-calcularFila(Pos1,Fila1,Pos2,Fila2),Fila1 =:= Fila2.



filaAbajo(Pos1,Pos2) :-filaPos(Pos1,Res1),filaPos(Pos2,Res2),Res2 is Res1+1.

filaArriba(Pos1,Pos2) :-filaPos(Pos1,Res1),filaPos(Pos2,Res2),Res2 is Res1-1.

adyacenteArriba(Grid,Elemento, Pos,Lista,ListaProcesada) :- PosAux is Pos-5, PosAux>=0,not(member(PosAux,Lista)),nth0(PosAux, Grid, Achequear), Elemento=:=Achequear,adyacentes(Grid,Elemento,PosAux,[PosAux|Lista],ListaProcesada).
adyacenteArriba(_Grid, _Elemento, _Pos,Lista,Lista).

adyacenteAbajo(Grid,Elemento, Pos,Lista,ListaProcesada):-PosAux is Pos+5,PosAux<40,not(member(PosAux,Lista)),nth0(PosAux,Grid,Achequear),Elemento=:=Achequear,adyacentes(Grid,Elemento,PosAux,[PosAux|Lista],ListaProcesada).
adyacenteAbajo(_Grid, _Elemento, _Pos,Lista,Lista).

adyacenteIzquierda(Grid,Elemento, Pos,Lista,ListaProcesada):-PosAux is Pos-1,PosAux>=0,not(member(PosAux,Lista)),  nth0(PosAux,Grid,Achequear),Elemento=:=Achequear,mismaFila(Pos, PosAux),adyacentes(Grid,Elemento,PosAux,[PosAux|Lista],ListaProcesada).
adyacenteIzquierda(_Grid, _Elemento, _Pos,Lista,Lista).

adyacenteDerecha(Grid,Elemento, Pos,Lista,ListaProcesada):-PosAux is Pos+1,PosAux<40,not(member(PosAux,Lista)),  nth0(PosAux,Grid,Achequear),Elemento=:=Achequear,mismaFila(Pos, PosAux),adyacentes(Grid,Elemento,PosAux,[PosAux|Lista],ListaProcesada).
adyacenteDerecha(_Grid, _Elemento, _Pos,Lista,Lista).

adyacenteIzquierdaAbajo(Grid,Elemento, Pos,Lista,ListaProcesada) :-PosAux is Pos+4, PosAux<40,not(member(PosAux,Lista)), nth0(PosAux,Grid,Achequear),Elemento=:=Achequear, filaAbajo(Pos,PosAux),adyacentes(Grid,Elemento,PosAux,[PosAux|Lista],ListaProcesada).
adyacenteIzquierdaAbajo(_Grid, _Elemento, _Pos,Lista,Lista).

adyacenteIzquierdaArriba(Grid,Elemento, Pos,Lista,ListaProcesada) :- PosAux is Pos-6, PosAux>=0,not(member(PosAux,Lista)), nth0(PosAux,Grid,Achequear),Elemento=:=Achequear, filaArriba(Pos, PosAux),adyacentes(Grid,Elemento,PosAux,[PosAux|Lista],ListaProcesada).
adyacenteIzquierdaArriba(_Grid, _Elemento, _Pos,Lista,Lista).

adyacenteDerechaAbajo(Grid,Elemento, Pos,Lista,ListaProcesada) :- PosAux is Pos+6,PosAux<40,not(member(PosAux,Lista)),nth0(PosAux,Grid,Achequear),Elemento=:=Achequear, filaAbajo(Pos,PosAux),adyacentes(Grid,Elemento,PosAux,[PosAux|Lista],ListaProcesada).
adyacenteDerechaAbajo(_Grid, _Elemento, _Pos,Lista,Lista).

adyacenteDerechaArriba(Grid,Elemento, Pos,Lista,ListaProcesada) :- PosAux is Pos-4,PosAux>0,not(member(PosAux,Lista)), nth0(PosAux,Grid,Achequear),Elemento=:=Achequear,filaArriba(Pos, PosAux),adyacentes(Grid,Elemento,PosAux,[PosAux|Lista],ListaProcesada).
adyacenteDerechaArriba(_Grid, _Elemento, _Pos,Lista,Lista).

adyacentes(Grid, ElemInPos, Pos,Lista, ListaRes8) :-
    adyacenteDerechaAbajo(Grid, ElemInPos, Pos,Lista,ListaRes1),adyacenteAbajo(Grid,ElemInPos,Pos,ListaRes1,ListaRes2),adyacenteIzquierdaAbajo(Grid, ElemInPos, Pos, ListaRes2,ListaRes3),
    adyacenteDerecha(Grid, ElemInPos, Pos, ListaRes3,ListaRes4), adyacenteIzquierda(Grid, ElemInPos, Pos, ListaRes4,ListaRes5),
    adyacenteDerechaArriba(Grid, ElemInPos, Pos,ListaRes5,ListaRes6),adyacenteArriba(Grid, ElemInPos, Pos, ListaRes6,ListaRes7),adyacenteIzquierdaArriba(Grid, ElemInPos, Pos,ListaRes7,ListaRes8).
    
sacarGrupo(Grid,Pos,Res):-nth0(Pos,Grid,ElemInPos),adyacentes(Grid,ElemInPos,Pos,[],Res).