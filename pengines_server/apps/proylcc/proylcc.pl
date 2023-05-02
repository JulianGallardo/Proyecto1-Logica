:- module(proylcc, 
	[  
		join/4,
        powerUp/2
	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

 join(Grid, NumOfColumns, Path, RGrids):- calcularSumaPath(Grid, NumOfColumns, Path, Suma), calcularProximaPotencia(Suma, 2, Res), generarLista(Grid, NumOfColumns, Path,Res,GridA),
 grillasConGravedad(GridA,[GridA],ListaResultante),RGrids = ListaResultante.


/**
* sacarIDelPath(+PosicionGrilla,+NumOfColumns,-ResPos)
 * sacarIDelPath es un predicado, el cual le pasas el conjunto [X,Y] de un elemento del path y te calcula el
 * indice de la lista de la Grilla. Ejemplo, Conjunto [2,3] devuelve 2+NumOfColumns*3 
 **/
sacarIDelPath([X], _NumOfColumns, X).
sacarIDelPath([X|Xs], NumOfColumns, Res1) :- sacarIDelPath(Xs, NumOfColumns, ResAux), Res1 is ResAux+(X*NumOfColumns).


/**
 * borrarElementoI(+Lista,+Indice,-ListaConIndiceBorrado)
 * ListaConIndiceBorrado es la Lista con el elemento en el Indice convertido en 0.  
 **/
borrarElementoI([_X|Xs], 0, [0|Xs]).
borrarElementoI([X|Xs], Y, [X|Res]):- YAux is Y-1, borrarElementoI(Xs, YAux, Res).


/**
 * reemplazarElementoI(+Lista,+Indice,+ElementoAPoner,-ListaProcesada)
 * ListaProcesada es la Lista pasada, con el elemento en el Indice reemplazado por ElementoAPoner. 
 **/
reemplazarElementoI([_X|Xs], 0, Z, [Z|Xs]).
reemplazarElementoI([X|Xs], Y, Z, [X|Res]):- YAux is Y-1, reemplazarElementoI(Xs, YAux, Z, Res).

/**
 * generarLista(+Grid,+NumOfColumns,+Path,-GridProcesada)
 * GridProcesada es una Grilla en la cual los elementos del Path son eliminados en Grid.
 **/
generarLista(Grid, NumOfColumns, [X],Suma, GridRes) :- sacarIDelPath(X, NumOfColumns, PosI), reemplazarElementoI(Grid, PosI, Suma, GridRes).
generarLista(Grid, NumOfColumns, [X|Xs],Suma, GridRes) :- generarLista(Grid, NumOfColumns, Xs,Suma,GridAux), 
			 sacarIDelPath(X, NumOfColumns, PosI), borrarElementoI(GridAux, PosI, GridRes).

/**
 * buscarElementoI(+Lista,+Indice,-ElementoBuscado)
 * ElementoBuscado es el elemento en el Indice pasado en la Lista.
 * El indice comienza desde 0.
 **/
buscarElementoI([X|_XS], 0, X).
buscarElementoI([_X|Xs], Y, Res):- YAux is Y-1, buscarElementoI(Xs,YAux,Res).			

/**
 * calcularSumaPath(+Grid,+NumOfColumns,+Path,-ResultadoSuma)
 * ResultadoSuma es el resultado de sumar todos los elementos señalados por cada posicion del Path en la Grid. 
 **/
calcularSumaPath(Grid, NumOfColumns, [X], Res) :- sacarIDelPath(X, NumOfColumns, PosI), buscarElementoI(Grid, PosI, Res).
calcularSumaPath(Grid, NumOfColumns, [X|Xs], Res) :- calcularSumaPath(Grid, NumOfColumns, Xs, ResAux),
 sacarIDelPath(X, NumOfColumns, PosI), buscarElementoI(Grid, PosI, ResAux2), Res is ResAux+ResAux2.

/**
 * calcularProximaPotencia(+Numero,+Y,-ProximaPotencia)
 * ProximaPotencia es la proxima potencia de 2, del Numero pasado.
 * Y es el exponente.
 **/
calcularProximaPotencia(Numero, Y, ProximaPotencia) :- Numero>(2**Y), Y2 is Y+1, calcularProximaPotencia(Numero, Y2, ProximaPotencia).
calcularProximaPotencia(Numero, Y, ProximaPotencia) :- Numero=<(2**Y), ProximaPotencia is 2**Y.

/**
 * generarPotencia2Random(-Resultado)
 * Resultado es una potencia 2 generada aleatoriamente entre un exponente 1 y 9.
 **/
generarPotencia2Random(Resultado) :-
    random(1, 9, Exponente),  % Genera un exponente aleatorio entre 1 y 9
    Resultado is 2 ** Exponente.

/**
 *sacarColumnaI(+Grid,+Contador,+Numero,+NumMod,-Columna)
 * Columna es la columna Numero en la Grilla Grid.     
 * Por Ejemplo si le pasamos sacarColumnaI(+Grid,+Contador,4,5,-Columna) en una grilla de 5 columnas, nos devuelve la columna de mas a la derecha.
 **/
sacarColumnaI([_X], Contador, Numero, NumMod, Res) :- Numero =\= Contador mod NumMod, Res = [].
sacarColumnaI([X], Contador, Numero, NumMod, Res) :- Numero =:= Contador mod NumMod, Res = [X].
sacarColumnaI([X|Xs], Contador, Numero, NumMod, [X|ResAux]) :-  Numero =:= Contador mod NumMod, Contador2 is Contador+1, sacarColumnaI(Xs, Contador2, Numero, NumMod, ResAux).
sacarColumnaI([_|Xs], Contador, Numero, NumMod, Res) :-  Numero =\= Contador mod NumMod, Contador2 is Contador+1, sacarColumnaI(Xs, Contador2, Numero, NumMod, Res).


/**
 * borrarElemento(+Lista,+Pos,-ListaRes)
 * ListaRes es la lista con el elemento en el indice Pos eliminado. 
 * El tamaño de ListaRes es la longitud de Lista menos 1.
 **/
borrarElemento([_X|Xs], 0, Xs).
borrarElemento([X|Xs], Y, [X|Res]):- YAux is Y-1, borrarElemento(Xs, YAux, Res).

ultimaAparicion0(Lista, UltimaAparicion) :-
    reverse(Lista, Reversa),           % invertir la lista
    append(_, [0|Resto], Reversa),     % buscar la última aparición de 0 en la lista invertida
    length(Resto, LongitudResto),      % obtener la longitud de la parte de la lista que sigue a la última aparición
    UltimaAparicion is LongitudResto.


/**
 * reemplazar0(+Lista,-ListaProcesada)
 * ListaProcesada es la Lista con la ultima aparicion de 0 borrada y reemplazado por una potencia de 2 al principio de la lista.
 **/
reemplazar0(Lista,[Pot|Res]):-ultimaAparicion0(Lista,UltimaAparicion),borrarElemento(Lista,UltimaAparicion,Res),generarPotencia2Random(Pot).
reemplazar0(Lista,Lista).


/**
* ordenarColumnas(+Grid,+Contador,-Res)
 * Res es una lista con todas las columnas de Grid concatenadas empezando de la 4 hasta la 0.
 * 
 **/
ordenarColumnas(Grid, 4, Res) :- sacarColumnaI(Grid, 0, 4, 5, ResAux1), reemplazar0(ResAux1,Res).
ordenarColumnas(Grid, Contador, Res) :- ContadorAux is Contador+1,ordenarColumnas(Grid,ContadorAux,ColumnasOrdenadas),sacarColumnaI(Grid, 0, Contador,5, ResAux1), reemplazar0(ResAux1,ResAux3),append(ResAux3,ColumnasOrdenadas,Res).


/**
 * ponerColumnasBien(+ColumnasConcatenadas,+Cont,-Res)
 * Res es la grilla armada devuelta a partir de agarrar la lista con las columnas concatenadas.
 **/
ponerColumnasBien(ColumnasConcatenadas, 8, Res):-sacarColumnaI(ColumnasConcatenadas,0,8,8,Res).
ponerColumnasBien(ColumnasConcatenadas, Contador, Res) :- ContadorAux is Contador+1,ponerColumnasBien(ColumnasConcatenadas,ContadorAux,ResAux2),
 sacarColumnaI(ColumnasConcatenadas, 0, Contador,8, ResAux1), append(ResAux1, ResAux2, Res).

/**
 *gravedad(+Grid,-Res) 
 * Res es la grilla Grid luego de aplicar la gravedad 1 vez, es decir que los elementos son desplazados 1 cuadrado para abajo en las columnas donde hay un 0, y reemplaza el primer elemento de la colummna
 * por una potencia de 2 random. 
 **/
gravedad(Grid, Res):- ordenarColumnas(Grid,0, ResAux),ponerColumnasBien(ResAux,0, Res).


/**
 * grillasConGravedad(+Grid,+ListaResultante,-ListaRec)
 * ListaRec es la lista que contiene las grillas luego de aplicar gravedad hasta que no sea posible aplicar devuelta.
 **/
grillasConGravedad(Grid,ListaResultante,ListaRec):-member(0,Grid),gravedad(Grid,GrillaConGravedad),append(ListaResultante, [GrillaConGravedad], Resultado),grillasConGravedad(GrillaConGravedad,Resultado,ListaRec).
grillasConGravedad(Grid,ListaResultante,ListaResultante):-not(member(0,Grid)). 


%Empieza_el_booster


/**
 * filaPos(+Pos,-ResFila)
 * ResFila es el numero de la fila de la posicion Pos.
 **/
filaPos(Pos,0):-Pos<5,Pos>=0.
filaPos(Pos,ResFila):-PosAux is Pos-5,Pos>=0,filaPos(PosAux,ResAux),ResFila is ResAux+1.

/**
 * mismaFila(+Pos1,+Pos2)
 * Devuelve true si Pos1 y Pos2 son de la misma fila, sino false.
 **/
mismaFila(Pos1,Pos2):-filaPos(Pos1,Fila1),filaPos(Pos2,Fila2),Fila1 =:= Fila2.


/**
 * filaAbajo(+Pos1,+Pos2)
 * Devuelve true si la Pos1 esta una fila abajo de Pos2, sino false.
 **/
filaAbajo(Pos1,Pos2) :-filaPos(Pos1,Res1),filaPos(Pos2,Res2),Res2 is Res1+1.

/**
 * filaArriba(+Pos1,+Pos2)
 * Devuelve true si la Pos1 esta una fila arriba de Pos2, sino false.
 **/
filaArriba(Pos1,Pos2) :-filaPos(Pos1,Res1),filaPos(Pos2,Res2),Res2 is Res1-1.

/**
 * adyacenteArriba(+Grid,+Elemento,+Pos,+Lista,-ListaProcesada)
 * ListaProcesada es el resultado de procesar el adyacente de Arriba de la posicion Pos, si el elemento en la posicion del adyacente es igual al elemento Elemento, entonces se buscan los
 * adyacentes del adyacente. 
 **/
adyacenteArriba(Grid,Elemento, Pos,Lista,ListaProcesada) :- PosAux is Pos-5, PosAux>=0,not(member(PosAux,Lista)),nth0(PosAux, Grid, Achequear), Elemento=:=Achequear,adyacentes(Grid,Elemento,PosAux,[PosAux|Lista],ListaProcesada).
adyacenteArriba(_Grid, _Elemento, _Pos,Lista,Lista).

/**
 * adyacenteAbajo(+Grid,+Elemento,+Pos,+Lista,-ListaProcesada)
 * ListaProcesada es el resultado de procesar el adyacente de Abajo de la posicion Pos, si el elemento en la posicion del adyacente es igual al elemento Elemento, entonces se buscan los
 * adyacentes del adyacente. 
 **/
adyacenteAbajo(Grid,Elemento, Pos,Lista,ListaProcesada):-PosAux is Pos+5,PosAux<40,not(member(PosAux,Lista)),nth0(PosAux,Grid,Achequear),Elemento=:=Achequear,adyacentes(Grid,Elemento,PosAux,[PosAux|Lista],ListaProcesada).
adyacenteAbajo(_Grid, _Elemento, _Pos,Lista,Lista).

/**
 * adyacenteIzquierda(+Grid,+Elemento,+Pos,+Lista,-ListaProcesada)
 * ListaProcesada es el resultado de procesar el adyacente de la Izquierda de la posicion Pos, si el elemento en la posicion del adyacente es igual al elemento Elemento, entonces se buscan los
 * adyacentes del adyacente. 
 * 
 **/
adyacenteIzquierda(Grid,Elemento, Pos,Lista,ListaProcesada):-PosAux is Pos-1,PosAux>=0,not(member(PosAux,Lista)),  nth0(PosAux,Grid,Achequear),Elemento=:=Achequear,mismaFila(Pos, PosAux),adyacentes(Grid,Elemento,PosAux,[PosAux|Lista],ListaProcesada).
adyacenteIzquierda(_Grid, _Elemento, _Pos,Lista,Lista).

/**
 * adyacenteDerecha(+Grid,+Elemento,+Pos,+Lista,-ListaProcesada)
 * ListaProcesada es el resultado de procesar el adyacente de la Derecha de la posicion Pos, si el elemento en la posicion del adyacente es igual al elemento Elemento, entonces se buscan los
 * adyacentes del adyacente.
 * 
 **/
adyacenteDerecha(Grid,Elemento, Pos,Lista,ListaProcesada):-PosAux is Pos+1,PosAux<40,not(member(PosAux,Lista)),  nth0(PosAux,Grid,Achequear),Elemento=:=Achequear,mismaFila(Pos, PosAux),adyacentes(Grid,Elemento,PosAux,[PosAux|Lista],ListaProcesada).
adyacenteDerecha(_Grid, _Elemento, _Pos,Lista,Lista).

/**
 * adyacenteIzquierdaAbajo(+Grid,+Elemento,+Pos,+Lista,-ListaProcesada)
 * ListaProcesada es el resultado de procesar el adyacente inferior izquierdo de la posicion Pos, si el elemento en la posicion del adyacente es igual al elemento Elemento, entonces se buscan los
 * adyacentes del adyacente.
 * 
 **/
adyacenteIzquierdaAbajo(Grid,Elemento, Pos,Lista,ListaProcesada) :-PosAux is Pos+4, PosAux<40,not(member(PosAux,Lista)), nth0(PosAux,Grid,Achequear),Elemento=:=Achequear, filaAbajo(Pos,PosAux),adyacentes(Grid,Elemento,PosAux,[PosAux|Lista],ListaProcesada).
adyacenteIzquierdaAbajo(_Grid, _Elemento, _Pos,Lista,Lista).

/**
 * adyacenteIzquierdaAbajo(+Grid,+Elemento,+Pos,+Lista,-ListaProcesada)
 * ListaProcesada es el resultado de procesar el adyacente superior izquierdo de la posicion Pos, si el elemento en la posicion del adyacente es igual al elemento Elemento, entonces se buscan los
 * adyacentes del adyacente.
 * 
 **/
adyacenteIzquierdaArriba(Grid,Elemento, Pos,Lista,ListaProcesada) :- PosAux is Pos-6, PosAux>=0,not(member(PosAux,Lista)), nth0(PosAux,Grid,Achequear),Elemento=:=Achequear, filaArriba(Pos, PosAux),adyacentes(Grid,Elemento,PosAux,[PosAux|Lista],ListaProcesada).
adyacenteIzquierdaArriba(_Grid, _Elemento, _Pos,Lista,Lista).

/**
 * adyacenteIzquierdaAbajo(+Grid,+Elemento,+Pos,+Lista,-ListaProcesada)
 * ListaProcesada es el resultado de procesar el adyacente inferior derecho de la posicion Pos, si el elemento en la posicion del adyacente es igual al elemento Elemento, entonces se buscan los
 * adyacentes del adyacente.
 * 
 **/
adyacenteDerechaAbajo(Grid,Elemento, Pos,Lista,ListaProcesada) :- PosAux is Pos+6,PosAux<40,not(member(PosAux,Lista)),nth0(PosAux,Grid,Achequear),Elemento=:=Achequear, filaAbajo(Pos,PosAux),adyacentes(Grid,Elemento,PosAux,[PosAux|Lista],ListaProcesada).
adyacenteDerechaAbajo(_Grid, _Elemento, _Pos,Lista,Lista).

/**
 * adyacenteIzquierdaAbajo(+Grid,+Elemento,+Pos,+Lista,-ListaProcesada)
 * ListaProcesada es el resultado de procesar el adyacente superior derecho de la posicion Pos, si el elemento en la posicion del adyacente es igual al elemento Elemento, entonces se buscan los
 * adyacentes del adyacente.
 * 
 **/
adyacenteDerechaArriba(Grid,Elemento, Pos,Lista,ListaProcesada) :- PosAux is Pos-4,PosAux>0,not(member(PosAux,Lista)), nth0(PosAux,Grid,Achequear),Elemento=:=Achequear,filaArriba(Pos, PosAux),adyacentes(Grid,Elemento,PosAux,[PosAux|Lista],ListaProcesada).
adyacenteDerechaArriba(_Grid, _Elemento, _Pos,Lista,Lista).

/**
 * adyacentes(+Grid,+ElemInPos,+Pos,+Lista,+ListaRes8)
 * ListaRes8 es la lista procesada luego de calcular todos los adyacentes de la posicion Pos y sus adyacentes en la grilla Grid.
 * Un adyacente se guarda en la lista de adyacentes solo si el elemento en la posicon adyacente es igual al elemento ElemInPos.
 **/
adyacentes(Grid, ElemInPos, Pos,Lista, ListaRes8) :-
    adyacenteDerechaAbajo(Grid, ElemInPos, Pos,Lista,ListaRes1),adyacenteAbajo(Grid,ElemInPos,Pos,ListaRes1,ListaRes2),adyacenteIzquierdaAbajo(Grid, ElemInPos, Pos, ListaRes2,ListaRes3),
    adyacenteDerecha(Grid, ElemInPos, Pos, ListaRes3,ListaRes4), adyacenteIzquierda(Grid, ElemInPos, Pos, ListaRes4,ListaRes5),
    adyacenteDerechaArriba(Grid, ElemInPos, Pos,ListaRes5,ListaRes6),adyacenteArriba(Grid, ElemInPos, Pos, ListaRes6,ListaRes7),adyacenteIzquierdaArriba(Grid, ElemInPos, Pos,ListaRes7,ListaRes8).


/**
 * sacarGrupo(+Grid,+Pos,-Res)
 * Res es una lista con los elementos de un grupo , empezando por la posicion Pos de la grilla Grid. 
 **/ 
sacarGrupo(Grid,Pos,Res):-nth0(Pos,Grid,ElemInPos),adyacentes(Grid,ElemInPos,Pos,[],Res).

 /**
* estaContenida(+ListaDeListas,+ListaBuscada)
 * Devuelve true si la ListaBuscada es igual a alguna de las Listas en la ListaDeListas, no importa si los elementos no tiene el mismo orden.
 **/ 
estaContenida([ListaCabeza|_SubListas], ListaBuscada) :-mismaLista(ListaCabeza,ListaBuscada).
estaContenida([_ListaCabeza|SubListas], ListaBuscada) :-estaContenida(SubListas,ListaBuscada).
    
/**
 * mismaLista(+ListaAComprobar,+Lista)
 * Devuelve true si ambas listas tienen los mismos elementos.
 **/ 
mismaLista(ListaAComprobar,[X]):-member(X,ListaAComprobar).
mismaLista(ListaAComprobar,[X|Xs]):-member(X,ListaAComprobar),mismaLista(ListaAComprobar,Xs).

/**
 * 
 * VER SI NO SE PUEDE REEMPLAZAR POR EL HECHO ANTES EN EL JOIN YA QUE HACEN LO MISMO PERO UNO CON PATH Y EL OTRO CON INDICES DE LA GRILLA
 **/

calcularSuma(Grid, [X], Res) :- buscarElementoI(Grid, X, Res).
calcularSuma(Grid, [X|Xs], Res) :- calcularSuma(Grid, Xs, ResAux),buscarElementoI(Grid, X, ResAux2), Res is ResAux+ResAux2.


/**
 * sacarGruposGrilla(+Grid,+ListaGrupos,-ListaDeGruposFinal)
 * ListaDeGruposFinal es una lista la cual contiene todos los grupos de la grilla.
 * ListaGrupos es una lista la cual va guardando los grupos armados, para no tener duplicados.
 **/ 
sacarGruposGrilla(Grid, 39, ListaGrupos, [ResAux|ListaGrupos]) :- sacarGrupo(Grid, 39, ResAux),length(ResAux,Long),Long>1,not(estaContenida(ListaGrupos,ResAux)).
sacarGruposGrilla(_Grid, 39, ListaGrupos, ListaGrupos).
sacarGruposGrilla(Grid, Posicion, ListaGrupos, ListaDeGruposFinal) :- sacarGrupo(Grid, Posicion, ResAux),length(ResAux,Long),Long>1, not(estaContenida(ListaGrupos,ResAux)), Posicion2 is Posicion+1, sacarGruposGrilla(Grid, Posicion2,[ResAux|ListaGrupos],ListaDeGruposFinal).
sacarGruposGrilla(Grid, Posicion, ListaGrupos, ListaDeGruposFinal) :- Posicion2 is Posicion+1, sacarGruposGrilla(Grid, Posicion2, ListaGrupos, ListaDeGruposFinal).


/**
 * reemplazarElemetosGrupo(+Grid,+ListaGrupo,+Max,+ProximaPotencia,-GridRes)
 * GridRes es una grilla en la cual los elementos de ListaGrupo son reemplazados por 0, salvo el mayor Max, que es reemplazado por ProxPotencia, ya que seria el elemento de mas 
 * abajo a la derecha. ProxPotencia es la proxima potencia de 2, de la suma de los elementos del grupo.
 **/     
reemplazarElementosGrupo(Grid,[X],Max,ProxPotencia,GridRes):-X=Max,reemplazarElementoI(Grid,X,ProxPotencia,GridRes).     
reemplazarElementosGrupo(Grid,[X],Max,_ProxPotencia,GridRes):-X\=Max,reemplazarElementoI(Grid,X,0,GridRes). 
reemplazarElementosGrupo(Grid,[X|Xs],Max,ProxPotencia,GridRes):-X=Max,reemplazarElementoI(Grid,X,ProxPotencia,GridAux),reemplazarElementosGrupo(GridAux,Xs,Max,ProxPotencia,GridRes).
reemplazarElementosGrupo(Grid,[X|Xs],Max,ProxPotencia,GridRes):-X\=Max,reemplazarElementoI(Grid,X,0,GridAux),reemplazarElementosGrupo(GridAux,Xs,Max,ProxPotencia,GridRes).    

/**
 * procesarEliminacionGrupos(+Grid,+ListaDeGrupos,-GridRes)
 * GridRes es la grilla resultante de procesar todos los Grupos en ListaDeGrupos. 
 **/ 
procesarEliminacionGrupos(Grid,[ListaCabeza],GridRes):-max_list(ListaCabeza,Maximo),calcularSuma(Grid,ListaCabeza,ResultadoSuma),calcularProximaPotencia(ResultadoSuma, 2, Res),reemplazarElementosGrupo(Grid,ListaCabeza,Maximo,Res,GridRes).
procesarEliminacionGrupos(Grid,[ListaCabeza|RestoListas],GridRes):-max_list(ListaCabeza,Maximo),calcularSuma(Grid,ListaCabeza,ResultadoSuma),calcularProximaPotencia(ResultadoSuma, 2, Res),reemplazarElementosGrupo(Grid,ListaCabeza,Maximo,Res,GridAux),procesarEliminacionGrupos(GridAux,RestoListas,GridRes).

/**
 * borrarGrupos(+Grid,-GridRes)
 * GridRes es la grilla con todos los grupos procesados para el ColapsarIguales.
 * Primero se sacan todos los grupos de la grilla y despues se procesan.
 * 
 **/ 
borrarGrupos(Grid,GridRes):-sacarGruposGrilla(Grid,0,[],ListaGrupos),procesarEliminacionGrupos(Grid,ListaGrupos,GridRes).


/**
 * powerUp(+Grid,-RGrid)
 * RGrid es una lista con los pasos que hace la grilla para aplicar el powerUp
 * El numero 0 en las celdas reperesenta una celda vacia.
 **/ 
powerUp(Grid,RGrids):-borrarGrupos(Grid,GridA),grillasConGravedad(GridA,[GridA],RGrids).
