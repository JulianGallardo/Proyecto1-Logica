:- module(proylcc, 
	[  
		join/5,
        powerUp/4,
        ayudaMovidaMaxima/4,
        ayudaMaximosIgualesAdyacentes/4
	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

 join(Grid, NumOfColumns,NumOfRows, Path, RGrids):- calcularSumaPath(Grid, NumOfColumns, Path, Suma), calcularProximaPotencia(Suma, 2, ProximaPotencia), generarLista(Grid, NumOfColumns, Path,ProximaPotencia,GridA),
 grillasConGravedad(GridA,[GridA],NumOfColumns,NumOfRows,ListaGrillasResultante),RGrids = ListaGrillasResultante.


/**
* sacarIDelPath(+PosicionGrilla,+NumOfColumns,-ResPos)
 * sacarIDelPath es un predicado, el cual le pasas el conjunto [X,Y] de un elemento del path y te calcula el
 * indice de la lista de la Grilla. Ejemplo, Conjunto [2,3] devuelve 2+NumOfColumns*3 
 **/
sacarIDelPath([IndiceColumna], _NumOfColumns, IndiceColumna).
sacarIDelPath([IndiceFila|Columna], NumOfColumns, IndiceGrilla) :- sacarIDelPath(Columna, NumOfColumns, IndiceColumna), IndiceGrilla is IndiceColumna+(IndiceFila*NumOfColumns).


/**
 * borrarElementoI(+Lista,+Indice,-ListaConIndiceBorrado)
 * ListaConIndiceBorrado es la Lista con el elemento en el Indice convertido en 0.  
 **/
borrarElementoI([_X|Resto], 0, [0|Resto]).
borrarElementoI([X|Resto], Cont, [X|ResultadoEliminacion]):- ContMenos1 is Cont-1, borrarElementoI(Resto, ContMenos1, ResultadoEliminacion).


/**
 * reemplazarElementoI(+Lista,+Indice,+ElementoAPoner,-ListaProcesada)
 * ListaProcesada es la Lista pasada, con el elemento en el Indice reemplazado por ElementoAPoner. 
 **/
reemplazarElementoI([_X|Resto], 0, ElementoAInsertar, [ElementoAInsertar|Resto]).
reemplazarElementoI([X|Resto], Cont, ElementoAInsertar, [X|Res]):- ContMenos1 is Cont-1, reemplazarElementoI(Resto, ContMenos1, ElementoAInsertar, Res).


/**
 * generarLista(+Grid,+NumOfColumns,+Path,-GridProcesada)
 * GridProcesada es una Grilla en la cual los elementos del Path son eliminados en Grid.
 **/
generarLista(Grid, NumOfColumns, [X],Suma, GridProcesada) :- sacarIDelPath(X, NumOfColumns, PosI), reemplazarElementoI(Grid, PosI, Suma, GridProcesada).
generarLista(Grid, NumOfColumns, [X|Sublista],Suma, GridProcesada) :- generarLista(Grid, NumOfColumns, Sublista,Suma,GridAux), 
			 sacarIDelPath(X, NumOfColumns, PosI), borrarElementoI(GridAux, PosI, GridProcesada).

/**
 * buscarElementoI(+Lista,+Indice,-ElementoBuscado)
 * ElementoBuscado es el elemento en el Indice pasado en la Lista.
 * El indice comienza desde 0.
 **/
buscarElementoI([ElementoBuscado|_SubLista], 0, ElementoBuscado).
buscarElementoI([_X|Sublista], Cont, ElementoBuscado):- ContMenos1 is Cont-1, buscarElementoI(Sublista,ContMenos1,ElementoBuscado).			


/**
 * pasarPathAListaIndices(+Path,+NumOfColumns,-ListaIndices)
 * ListaIndices es la lista con las posiciones del path convertidas a indices de una lista del 0 a X.
 * Por ejemplo si tenemos un elemento del path [1,2], el indice sera 7. 
 * */
pasarPathAListaIndices([X],NumOfColumns,[Res]):-sacarIDelPath(X,NumOfColumns,Res).
pasarPathAListaIndices([X|RestoPath],NumOfColumns,[Res|ListaIndices]):-pasarPathAListaIndices(RestoPath,NumOfColumns,ListaIndices),
    sacarIDelPath(X,NumOfColumns,Res).


/**
 * calcularSuma(+Grid,+ListaACalcular,-ResultadoSuma)
 * ListaACalcular es una lista con indices de la grilla
 * ResutladoSuma es el resultado de sumar todos los elementos de las posiciones en los indices de ListaACalcular.
 * */
calcularSuma(Grid, [X], ResultadoSuma) :- buscarElementoI(Grid, X, ResultadoSuma).
calcularSuma(Grid, [X|Xs], ResultadoSuma) :- calcularSuma(Grid, Xs, ResultadoSumaAux),buscarElementoI(Grid, X, Elemento), ResultadoSuma is ResultadoSumaAux+Elemento.


/**
 * calcularSumaPath(+Grid,+NumOfColumns,+Path,-ResultadoSuma)
 * ResultadoSuma es el resultado de sumar todos los elementos señalados por cada posicion del Path en la Grid. 
 **/
calcularSumaPath(Grid, NumOfColumns, Path, ResultadoSuma) :-pasarPathAListaIndices(Path,NumOfColumns,PathConIndices),calcularSuma(Grid,PathConIndices,ResultadoSuma). 

/**
 * calcularProximaPotencia(+Numero,+Exponente,-ProximaPotencia)
 * ProximaPotencia es la proxima potencia de 2, del Numero pasado.
 * Y es el exponente.
 **/
calcularProximaPotencia(Numero, Exponente, ProximaPotencia) :- Numero>(2**Exponente), SiguienteExponente is Exponente+1, calcularProximaPotencia(Numero, SiguienteExponente, ProximaPotencia).
calcularProximaPotencia(Numero, Exponente, ProximaPotencia) :- Numero=<(2**Exponente), ProximaPotencia is 2**Exponente.

/**
 * generarPotencia2Random(-Resultado)
 * Resultado es una potencia 2 generada aleatoriamente entre un exponente 1 y 9.
 **/
generarPotencia2Random(Resultado) :-
    random(1, 9, Exponente),  % Genera un exponente aleatorio entre 1 y 9
    Resultado is 2 ** Exponente.

/**
 *sacarColumnaI(+Grid,+Contador,+ColumnaI,+CantidadColumnas,-ListaColumna)
 * Columna es la ColumnaI en la Grilla Grid.
     
 * Por Ejemplo si le pasamos sacarColumnaI(+Grid,+Contador,4,5,-Columna) en una grilla de 5 columnas, nos devuelve la columna de mas a la derecha.
 **/
sacarColumnaI([_X], Contador, ColumnaI, CantidadColumnas, []) :- ColumnaI =\= Contador mod CantidadColumnas.
sacarColumnaI([X], Contador, ColumnaI, CantidadColumnas, [X]) :- ColumnaI =:= Contador mod CantidadColumnas.
sacarColumnaI([X|SubLista], Contador, ColumnaI, CantidadColumnas, [X|ListaColumna]) :-  ColumnaI =:= Contador mod CantidadColumnas, Contador2 is Contador+1, sacarColumnaI(SubLista, Contador2, ColumnaI, CantidadColumnas, ListaColumna).
sacarColumnaI([_|SubLista], Contador, ColumnaI, CantidadColumnas, ListaColumna) :-  ColumnaI =\= Contador mod CantidadColumnas, Contador2 is Contador+1, sacarColumnaI(SubLista, Contador2, ColumnaI, CantidadColumnas, ListaColumna).


/**
 * borrarElemento(+Lista,+Pos,-ListaRes)
 * ListaRes es la lista con el elemento en el indice Pos eliminado. 
 * El tamaño de ListaRes es la longitud de Lista menos 1.
 **/
borrarElemento([_X|SubLista], 0, SubLista).
borrarElemento([X|SubLista], Y, [X|Res]):- YAux is Y-1, borrarElemento(SubLista, YAux, Res).

/**
 * ultimaAparicion0(+Lista, -PosicionUltimaAparicion)
 * PosicionUltimaAparicion es el indice del ultimo 0 en la lista.
 **/
ultimaAparicion0(Lista, UltimaAparicion) :-
    reverse(Lista, Reversa),           % invertir la lista
    append(_, [0|Resto], Reversa),     % buscar la última aparición de 0 en la lista invertida
    length(Resto, LongitudResto),      % obtener la longitud de la parte de la lista que sigue a la última aparición
    UltimaAparicion is LongitudResto.


/**
 * reemplazarUltimo0(+Lista,-ListaProcesada)
 * ListaProcesada es la Lista con la ultima aparicion de 0 borrada y reemplazado por una potencia de 2 al principio de la lista.
 **/
reemplazarUltimo0(Lista,[Pot|Res]):-ultimaAparicion0(Lista,UltimaAparicion),borrarElemento(Lista,UltimaAparicion,Res),generarPotencia2Random(Pot).
reemplazarUltimo0(Lista,Lista).


/**
* ordenarColumnas(+Grid,+Contador, +NumOfColumns, -Res)
 * Res es una lista con todas las columnas de Grid concatenadas empezando de la 4 hasta la 0.
 * 
 **/
ordenarColumnas(Grid, Contador,NumOfColumns, Res) :- Contador=:=(NumOfColumns-1),sacarColumnaI(Grid, 0, Contador, NumOfColumns, Columna), reemplazarUltimo0(Columna,Res).
ordenarColumnas(Grid, Contador,NumOfColumns, Res) :- ContadorAux is Contador+1,ordenarColumnas(Grid,ContadorAux,NumOfColumns,ColumnasOrdenadas),sacarColumnaI(Grid, 0, Contador,NumOfColumns, Columna), reemplazarUltimo0(Columna,ColumnaProcesada),append(ColumnaProcesada,ColumnasOrdenadas,Res).


/**
 * armarGrilla(+ColumnasConcatenadas,+Cont,+NumOfRows,-GrillaOrdenada)
 * GrillaOrdenada es la grilla armada devuelta a partir de agarrar la lista con las columnas concatenadas.
 **/
armarGrilla(ColumnasConcatenadas,Contador, NumOfRows, FilaOrdenada):-Contador=:=NumOfRows,sacarColumnaI(ColumnasConcatenadas,0,NumOfRows,NumOfRows,FilaOrdenada),!.
armarGrilla(ColumnasConcatenadas, Contador,NumOfRows, GrillaOrdenada) :- ContadorAux is Contador+1,armarGrilla(ColumnasConcatenadas,ContadorAux,NumOfRows,FilasOrdenadas),
 sacarColumnaI(ColumnasConcatenadas, 0, Contador,NumOfRows, Fila), append(Fila, FilasOrdenadas, GrillaOrdenada).

/**
 *gravedad(+Grid,+NumOfColumns,+NumOfRows,-Res) 
 * Res es la grilla Grid luego de aplicar la gravedad 1 vez, es decir que los elementos son desplazados 1 cuadrado para abajo en las columnas donde hay un 0, y reemplaza el primer elemento de la colummna
 * por una potencia de 2 random. 
 **/
gravedad(Grid,NumOfColumns,NumOfRows,Res):- ordenarColumnas(Grid,0,NumOfColumns, ResAux),armarGrilla(ResAux,0,NumOfRows,Res).


/**
 * grillasConGravedad(+Grid,+ListaResultante,+NumOfColumns,+NumOfRows,-ListaRes)
 * ListaRes es la lista que contiene las grillas luego de aplicar gravedad hasta que no sea posible aplicar devuelta.
 * Este predicado funcionaria como un while(member(0,Grid))
 **/
grillasConGravedad(Grid,ListaInicial,NumOfColumns,NumOfRows,ListaRes):-member(0,Grid),gravedad(Grid,NumOfColumns,NumOfRows,GrillaConGravedad),append(ListaInicial, [GrillaConGravedad], Resultado),grillasConGravedad(GrillaConGravedad,Resultado,NumOfColumns,NumOfRows,ListaRes).
grillasConGravedad(Grid,ListaInicial,_NumOfColumns,_NumOfRows,ListaInicial):-not(member(0,Grid)). 


%Empieza Colapsar Iguales


/**
 * filaPos(+Pos,-ResFila)
 * ResFila es el numero de la fila de la posicion Pos.
 **/
filaPos(Pos,0):-Pos<5,Pos>=0.
filaPos(Pos,Fila):-PosAux is Pos-5,Pos>=0,filaPos(PosAux,FilaAnterior),Fila is FilaAnterior+1.

/**
 * mismaFila(+Pos1,+Pos2)
 * Devuelve true si Pos1 y Pos2 son de la misma fila, sino false.
 **/
mismaFila(Pos1,Pos2):-filaPos(Pos1,Fila1),filaPos(Pos2,Fila2),Fila1 =:= Fila2.


/**
 * filaAbajo(+Pos1,+Pos2)
 * Devuelve true si la Pos1 esta una fila abajo de Pos2, sino false.
 **/
filaAbajo(Pos1,Pos2) :-filaPos(Pos1,FilaPos1),filaPos(Pos2,FilaPos2),FilaPos2 is FilaPos1+1.

/**
 * filaArriba(+Pos1,+Pos2)
 * Devuelve true si la Pos1 esta una fila arriba de Pos2, sino false.
 **/
filaArriba(Pos1,Pos2) :-filaPos(Pos1,FilaPos1),filaPos(Pos2,FilaPos2),FilaPos2 is FilaPos1-1.

/**
 * adyacenteArriba(+Grid,+Elemento,+Pos,+Lista,-ListaProcesada)
 * ListaProcesada es el resultado de procesar el adyacente de Arriba de la posicion Pos, si el elemento en la posicion del adyacente es igual al elemento Elemento, entonces se buscan los
 * adyacentes del adyacente. 
 **/
adyacenteArriba(Grid,Elemento, Pos,Lista,ListaProcesada) :- PosAdyacenteSuperior is Pos-5, PosAdyacenteSuperior>=0,not(member(PosAdyacenteSuperior,Lista)),nth0(PosAdyacenteSuperior, Grid, Achequear), Elemento=:=Achequear,adyacentes(Grid,Elemento,PosAdyacenteSuperior,[PosAdyacenteSuperior|Lista],ListaProcesada).
adyacenteArriba(_Grid, _Elemento, _Pos,Lista,Lista).

/**
 * adyacenteAbajo(+Grid,+Elemento,+Pos,+Lista,-ListaProcesada)
 * ListaProcesada es el resultado de procesar el adyacente de Abajo de la posicion Pos, si el elemento en la posicion del adyacente es igual al elemento Elemento, entonces se buscan los
 * adyacentes del adyacente. 
 **/
adyacenteAbajo(Grid,Elemento, Pos,Lista,ListaProcesada):-PosAdyacenteInferior is Pos+5,PosAdyacenteInferior<40,not(member(PosAdyacenteInferior,Lista)),nth0(PosAdyacenteInferior,Grid,Achequear),Elemento=:=Achequear,adyacentes(Grid,Elemento,PosAdyacenteInferior,[PosAdyacenteInferior|Lista],ListaProcesada).
adyacenteAbajo(_Grid, _Elemento, _Pos,Lista,Lista).


/**
 * adyacenteIzquierda(+Grid,+Elemento,+Pos,+Lista,-ListaProcesada)
 * ListaProcesada es el resultado de procesar el adyacente de la Izquierda de la posicion Pos, si el elemento en la posicion del adyacente es igual al elemento Elemento, entonces se buscan los
 * adyacentes del adyacente. 
 * 
 **/
adyacenteIzquierda(Grid,Elemento, Pos,Lista,ListaProcesada):-PosAdyacenteIzq is Pos-1,PosAdyacenteIzq>=0,not(member(PosAdyacenteIzq,Lista)),  nth0(PosAdyacenteIzq,Grid,Achequear),Elemento=:=Achequear,mismaFila(Pos, PosAdyacenteIzq),adyacentes(Grid,Elemento,PosAdyacenteIzq,[PosAdyacenteIzq|Lista],ListaProcesada).
adyacenteIzquierda(_Grid, _Elemento, _Pos,Lista,Lista).

/**
 * adyacenteDerecha(+Grid,+Elemento,+Pos,+Lista,-ListaProcesada)
 * ListaProcesada es el resultado de procesar el adyacente de la Derecha de la posicion Pos, si el elemento en la posicion del adyacente es igual al elemento Elemento, entonces se buscan los
 * adyacentes del adyacente.
 * 
 **/
adyacenteDerecha(Grid,Elemento, Pos,Lista,ListaProcesada):-PosAdyacenteDer is Pos+1,PosAdyacenteDer<40,not(member(PosAdyacenteDer,Lista)),  nth0(PosAdyacenteDer,Grid,Achequear),Elemento=:=Achequear,mismaFila(Pos, PosAdyacenteDer),adyacentes(Grid,Elemento,PosAdyacenteDer,[PosAdyacenteDer|Lista],ListaProcesada).
adyacenteDerecha(_Grid, _Elemento, _Pos,Lista,Lista).

/**
 * adyacenteIzquierdaAbajo(+Grid,+Elemento,+Pos,+Lista,-ListaProcesada)
 * ListaProcesada es el resultado de procesar el adyacente inferior izquierdo de la posicion Pos, si el elemento en la posicion del adyacente es igual al elemento Elemento, entonces se buscan los
 * adyacentes del adyacente.
 * 
 **/
adyacenteIzquierdaAbajo(Grid,Elemento, Pos,Lista,ListaProcesada) :-PosAdyInferiorIzq is Pos+4, PosAdyInferiorIzq<40,not(member(PosAdyInferiorIzq,Lista)), nth0(PosAdyInferiorIzq,Grid,Achequear),Elemento=:=Achequear, filaAbajo(Pos,PosAdyInferiorIzq),adyacentes(Grid,Elemento,PosAdyInferiorIzq,[PosAdyInferiorIzq|Lista],ListaProcesada).
adyacenteIzquierdaAbajo(_Grid, _Elemento, _Pos,Lista,Lista).

/**
 * adyacenteIzquierdaAbajo(+Grid,+Elemento,+Pos,+Lista,-ListaProcesada)
 * ListaProcesada es el resultado de procesar el adyacente superior izquierdo de la posicion Pos, si el elemento en la posicion del adyacente es igual al elemento Elemento, entonces se buscan los
 * adyacentes del adyacente.
 * 
 **/
adyacenteIzquierdaArriba(Grid,Elemento, Pos,Lista,ListaProcesada) :- PosAdySuperiorIzq is Pos-6, PosAdySuperiorIzq>=0,not(member(PosAdySuperiorIzq,Lista)), nth0(PosAdySuperiorIzq,Grid,Achequear),Elemento=:=Achequear, filaArriba(Pos, PosAdySuperiorIzq),adyacentes(Grid,Elemento,PosAdySuperiorIzq,[PosAdySuperiorIzq|Lista],ListaProcesada).
adyacenteIzquierdaArriba(_Grid, _Elemento, _Pos,Lista,Lista).

/**
 * adyacenteIzquierdaAbajo(+Grid,+Elemento,+Pos,+Lista,-ListaProcesada)
 * ListaProcesada es el resultado de procesar el adyacente inferior derecho de la posicion Pos, si el elemento en la posicion del adyacente es igual al elemento Elemento, entonces se buscan los
 * adyacentes del adyacente.
 * 
 **/
adyacenteDerechaAbajo(Grid,Elemento, Pos,Lista,ListaProcesada) :- PosAdyDerechaInferior is Pos+6,PosAdyDerechaInferior<40,not(member(PosAdyDerechaInferior,Lista)),nth0(PosAdyDerechaInferior,Grid,Achequear),Elemento=:=Achequear, filaAbajo(Pos,PosAdyDerechaInferior),adyacentes(Grid,Elemento,PosAdyDerechaInferior,[PosAdyDerechaInferior|Lista],ListaProcesada).
adyacenteDerechaAbajo(_Grid, _Elemento, _Pos,Lista,Lista).

/**
 * adyacenteIzquierdaAbajo(+Grid,+Elemento,+Pos,+Lista,-ListaProcesada)
 * ListaProcesada es el resultado de procesar el adyacente superior derecho de la posicion Pos, si el elemento en la posicion del adyacente es igual al elemento Elemento, entonces se buscan los
 * adyacentes del adyacente.
 * 
 **/
adyacenteDerechaArriba(Grid,Elemento, Pos,Lista,ListaProcesada) :- PosAdyDerechaSuperior is Pos-4,PosAdyDerechaSuperior>0,not(member(PosAdyDerechaSuperior,Lista)), nth0(PosAdyDerechaSuperior,Grid,Achequear),Elemento=:=Achequear,filaArriba(Pos, PosAdyDerechaSuperior),adyacentes(Grid,Elemento,PosAdyDerechaSuperior,[PosAdyDerechaSuperior|Lista],ListaProcesada).
adyacenteDerechaArriba(_Grid, _Elemento, _Pos,Lista,Lista).

/**
 * adyacentes(+Grid,+ElemInPos,+Pos,+Lista,-ListaAdyacentesFinal)
 * ListaAdyacentesFinal es la lista procesada luego de calcular todos los adyacentes de la posicion Pos y sus adyacentes en la grilla Grid.
 * Un adyacente se guarda en la lista de adyacentes solo si el elemento en la posicon adyacente es igual al elemento ElemInPos.
 **/
adyacentes(Grid, ElemInPos, Pos,Lista, ListaAdyacentesFinal) :-
    adyacenteDerechaAbajo(Grid, ElemInPos, Pos,Lista,ListaAdyacentes1),adyacenteAbajo(Grid,ElemInPos,Pos,ListaAdyacentes1,ListaAdyacentes2),adyacenteIzquierdaAbajo(Grid, ElemInPos, Pos, ListaAdyacentes2,ListaAdyacentes3),
    adyacenteDerecha(Grid, ElemInPos, Pos, ListaAdyacentes3,ListaAdyacentes4), adyacenteIzquierda(Grid, ElemInPos, Pos, ListaAdyacentes4,ListaAdyacentes5),
    adyacenteDerechaArriba(Grid, ElemInPos, Pos,ListaAdyacentes5,ListaAdyacentes6),adyacenteArriba(Grid, ElemInPos, Pos, ListaAdyacentes6,ListaAdyacentes7),adyacenteIzquierdaArriba(Grid, ElemInPos, Pos,ListaAdyacentes7,ListaAdyacentesFinal).


/**
 * sacarGrupo(+Grid,+Pos,-Res)
 * Res es una lista con los elementos de un grupo , empezando por la posicion Pos de la grilla Grid. 
 **/ 
sacarGrupo(Grid,Pos,ListaResultante):-nth0(Pos,Grid,ElemInPos),adyacentes(Grid,ElemInPos,Pos,[],ListaResultante).


/**
 * sacarGruposGrilla(+Grid,+Posicion,+;ListaVisitados,+ListaGrupos,-ListaDeGruposFinal)
 * ListaVisitados es una lista la cual guarda todos los indices ya visitados por los grupos.
 * ListaDeGruposFinal es una lista la cual contiene todos los grupos de la grilla.
 * ListaGrupos es una lista la cual va guardando los grupos armados, para no tener duplicados.
 **/ 
sacarGruposGrilla(Grid, 39, ListaVisitados,ListaGrupos, [Grupo|ListaGrupos]) :- not(member(39,ListaVisitados)),sacarGrupo(Grid, 39, Grupo),length(Grupo,Long),Long>1.
sacarGruposGrilla(_Grid, 39,_ListaVisitados, ListaGrupos, ListaGrupos).
sacarGruposGrilla(Grid, Posicion,ListaVisitados, ListaGrupos, ListaDeGruposFinal) :- not(member(Posicion,ListaVisitados)),sacarGrupo(Grid, Posicion, Grupo),length(Grupo,Long),Long>1, Posicion2 is Posicion+1, append(ListaVisitados,ListaGrupos,ListaVisitadosAux) ,sacarGruposGrilla(Grid, Posicion2,ListaVisitadosAux,[Grupo|ListaGrupos],ListaDeGruposFinal).
sacarGruposGrilla(Grid, Posicion,ListaVisitados, ListaGrupos, ListaDeGruposFinal) :- Posicion2 is Posicion+1, sacarGruposGrilla(Grid, Posicion2,ListaVisitados, ListaGrupos, ListaDeGruposFinal).


/**
 * reemplazarElemetosGrupo(+Grid,+ListaGrupo,+Max,+ProximaPotencia,-GridRes)
 * GridRes es una grilla en la cual los elementos de ListaGrupo son reemplazados por 0, salvo el mayor Max, que es reemplazado por ProxPotencia, ya que seria el elemento de mas 
 * abajo a la derecha. ProxPotencia es la proxima potencia de 2, de la suma de los elementos del grupo.
 **/     
reemplazarElementosGrupo(Grid,[X],Max,ProxPotencia,GridRes):-X=Max,reemplazarElementoI(Grid,X,ProxPotencia,GridRes).     
reemplazarElementosGrupo(Grid,[X],Max,_ProxPotencia,GridRes):-X\=Max,reemplazarElementoI(Grid,X,0,GridRes). 
reemplazarElementosGrupo(Grid,[X|RestoGrupo],Max,ProxPotencia,GridRes):-X=Max,reemplazarElementoI(Grid,X,ProxPotencia,GridAux),reemplazarElementosGrupo(GridAux,RestoGrupo,Max,ProxPotencia,GridRes).
reemplazarElementosGrupo(Grid,[X|RestoGrupo],Max,ProxPotencia,GridRes):-X\=Max,reemplazarElementoI(Grid,X,0,GridAux),reemplazarElementosGrupo(GridAux,RestoGrupo,Max,ProxPotencia,GridRes).    

/**
 * procesarEliminacionGrupos(+Grid,+ListaDeGrupos,-GridRes)
 * GridRes es la grilla resultante de procesar todos los Grupos en ListaDeGrupos. 
 **/ 
procesarEliminacionGrupos(Grid,[],Grid).
procesarEliminacionGrupos(Grid,[ListaCabeza],GridRes):-max_list(ListaCabeza,Maximo),calcularSuma(Grid,ListaCabeza,ResultadoSuma),calcularProximaPotencia(ResultadoSuma, 2, Res),reemplazarElementosGrupo(Grid,ListaCabeza,Maximo,Res,GridRes).
procesarEliminacionGrupos(Grid,[ListaCabeza|RestoListas],GridRes):-max_list(ListaCabeza,Maximo),calcularSuma(Grid,ListaCabeza,ResultadoSuma),calcularProximaPotencia(ResultadoSuma, 2, Res),reemplazarElementosGrupo(Grid,ListaCabeza,Maximo,Res,GridAux),procesarEliminacionGrupos(GridAux,RestoListas,GridRes).

/**
 * borrarGrupos(+Grid,-GridRes)
 * GridRes es la grilla con todos los grupos procesados para el ColapsarIguales.
 * Primero se sacan todos los grupos de la grilla y despues se procesan.
 * 
 **/ 
borrarGrupos(Grid,GridRes):-sacarGruposGrilla(Grid,0,[],[],ListaGrupos),procesarEliminacionGrupos(Grid,ListaGrupos,GridRes).


/**
 * powerUp(+Grid, +NumOfColumns, +NumOfRows, -RGrid)
 * RGrid es una lista con los pasos que hace la grilla para aplicar el powerUp
 * El numero 0 en las celdas reperesenta una celda vacia.
 **/ 
powerUp(Grid,NumOfColumns,NumOfRows,RGrids):-borrarGrupos(Grid,GridA),grillasConGravedad(GridA,[GridA],NumOfColumns,NumOfRows,RGrids).





/**
 * Empieza ayuda movida máxima
 **/ 

/**
 * mismaPotencia(+Elemento1, +Elemento2, +Base)
 **/ 
mismaPotencia(X,Y,_Base):-X =:= Y.

/**
 * potenciaSiguiente(+Elemento1, +Elemento2, +Potencia)
 * Verifica si X es la potencia siguiente de Y
 **/ 
potenciaSiguiente(X, Y, Pow) :-
    Pow =\= 1,        % Asegura que Pow no sea 1 (evita la repetición infinita)
    PowY is Y * Pow,  % Calcula la potencia siguiente de X
    PowY =:= X.       % Comprueba si la potencia siguiente de Y es igual a X
    
/**
 * cumpleCondicionesCamino(+PosicionSiguiente, +PosicionActual, +CaminoActual)
 * Verifica si la PosicionSiguiente puede agregarse a el CaminoActual 
 **/ 
cumpleCondicionesCamino(PosicionSiguiente,PosActual,CaminoActual):-
    length(CaminoActual,Long),Long=:=1,mismaPotencia(PosicionSiguiente,PosActual,2). %Caso inicio de Camino, camino de 1 elemento, ambos tienen que tener los mismos elementos.
cumpleCondicionesCamino(PosicionSiguiente,PosActual,CaminoActual):-length(CaminoActual,Long),Long>1,mismaPotencia(PosicionSiguiente,PosActual,2). %Camino de más de 1 elemento, ambos tienen que tener los mismos elementos.
cumpleCondicionesCamino(PosicionSiguiente,PosActual,CaminoActual):-length(CaminoActual,Long),Long>1,potenciaSiguiente(PosicionSiguiente,PosActual,2).%Camino de más de 1 elemento, PosSigueinte es la potencia siguiente de PosActual
                                
/**
 * adyacenteCamino(+Grid, +Elemento, +PosicionElemento, +PosicionAdyacente, +Condicion, +NumOfColumns, +NumOfRows, +CaminoActual, +CaminoMaximo, +ValorCaminoMaximo, -CaminoEncontrado, -ValorCaminoEncontrado)
 * La variable Condicion es aquella donde le pasaremos el predicado para verificar en el caso que corresponda la condicion definida por ellos.
 * Condicion puede ser los siguientes predicados: mismaFila,FilaArriba,FilaAbajo.
 * CaminoActual es una lista que contiene el camino que esta siendo analizado actualmente
 * CaminoMaximo contiene el camino con la sumatoria más grande encontrado hasta ahora
 * ValorCaminoMax contiene dicha sumatoria
 * CaminoEncontrado es el camino resultante que devolveremos cuando terminemos de recorrer todos los posibles caminos
 * ValorCaminoEncontrado es el valor de dicho camino
 * 
 **/ 
adyacenteCamino(Grid, ElemInPos, Pos,PosAdy,Condicion, NumOfColumns,NumOfRows,CaminoActual, CaminoMaximo, ValorCaminoMax, CaminoEncontrado, ValorCaminoEncontrado) :-
    PosAdy>=0,
    PosAdy<(NumOfColumns*NumOfRows),
    not(member(PosAdy, CaminoActual)),
    nth0(PosAdy, Grid, ElementoAdyacente),
    cumpleCondicionesCamino(ElementoAdyacente,ElemInPos,CaminoActual),call(Condicion,Pos,PosAdy),
    calcularSuma(Grid, [PosAdy|CaminoActual], ResSuma),
    (
        (
            ResSuma >= ValorCaminoMax,
            buscarCamino(Grid, ElementoAdyacente, PosAdy,NumOfColumns,NumOfRows, [PosAdy|CaminoActual], [PosAdy|CaminoActual], ResSuma, CaminoEncontrado, ValorCaminoEncontrado)
        );
        (
            buscarCamino(Grid, ElementoAdyacente, PosAdy,NumOfColumns,NumOfRows, [PosAdy|CaminoActual], CaminoMaximo, ValorCaminoMax, CaminoEncontrado, ValorCaminoEncontrado)
        )
    ).
adyacenteCamino(_Grid, _ElemInPos, _Pos,_PosAdy,_Condicion, _NumOfColumns,_NumOfRows, _CaminoActual,CaminoMaximo,ValorCaminoMaximo,CaminoMaximo,ValorCaminoMaximo).


/**
 * buscarCamino(+Grid, +Elemento, +PosicionElemento, +NumOfColumns, +NumOfRows, +CaminoActual, +CaminoMaximo, +ValorCaminoMaximo, -CaminoEncontrado, -ValorCaminoEncontrado)
 * CaminoActual es una lista que contiene el camino que esta siendo analizado actualmente
 * CaminoMaximo contiene el camino con la sumatoria más grande encontrado hasta ahora
 * ValorCaminoMax contiene dicha sumatoria
 * CaminoEncontrado es el camino resultante que devolveremos cuando terminemos de recorrer todos los posibles caminos
 * ValorCaminoEncontrado es el valor de dicho camino
 * 
 **/ 
buscarCamino(Grid, ElemInPos, Pos,NumOfColumns,NumOfRows, ListaVisitados, CaminoMaximo,ValorCaminoMaximo ,CaminoEncontrado8,ValorCaminoEncontrado8) :-
    %Adyacente abajo izquierda
    PosAdyAbajoIzquierda is Pos+NumOfColumns-1,
    adyacenteCamino(Grid, ElemInPos, Pos,PosAdyAbajoIzquierda,filaAbajo,NumOfColumns,NumOfRows,ListaVisitados,CaminoMaximo,ValorCaminoMaximo,CaminoEncontrado1,ValorCaminoEncontrado1),
    %Adyacente abajo
    PosAdyAbajo is Pos+NumOfColumns,
    adyacenteCamino(Grid, ElemInPos, Pos,PosAdyAbajo,filaAbajo,NumOfColumns,NumOfRows,ListaVisitados,CaminoEncontrado1,ValorCaminoEncontrado1,CaminoEncontrado2,ValorCaminoEncontrado2),
    %Adyacente abajo derecha
    PosAdyAbajoDerecha is Pos+NumOfColumns+1,
    adyacenteCamino(Grid, ElemInPos, Pos,PosAdyAbajoDerecha,filaAbajo,NumOfColumns,NumOfRows,ListaVisitados,CaminoEncontrado2,ValorCaminoEncontrado2,CaminoEncontrado3,ValorCaminoEncontrado3),
    %Adyacente izquierda
    PosAdyIzquierda is Pos+1,
    adyacenteCamino(Grid, ElemInPos, Pos,PosAdyIzquierda,mismaFila,NumOfColumns,NumOfRows,ListaVisitados,CaminoEncontrado3,ValorCaminoEncontrado3,CaminoEncontrado4,ValorCaminoEncontrado4),
    %Adyacente derecha
    PosAdyDerecha is Pos+1,
    adyacenteCamino(Grid, ElemInPos, Pos,PosAdyDerecha,mismaFila,NumOfColumns,NumOfRows,ListaVisitados,CaminoEncontrado4,ValorCaminoEncontrado4,CaminoEncontrado5,ValorCaminoEncontrado5),
    %Adyacente arriba izquierda
    PosAdyArribaIzquierda is Pos-NumOfColumns-1,
    adyacenteCamino(Grid, ElemInPos, Pos,PosAdyArribaIzquierda,filaArriba,NumOfColumns,NumOfRows,ListaVisitados,CaminoEncontrado5,ValorCaminoEncontrado5,CaminoEncontrado6,ValorCaminoEncontrado6),
    %Adyacente arriba
    PosAdyArriba is Pos-NumOfColumns,
    adyacenteCamino(Grid, ElemInPos, Pos,PosAdyArriba,filaArriba,NumOfColumns,NumOfRows,ListaVisitados,CaminoEncontrado6,ValorCaminoEncontrado6,CaminoEncontrado7,ValorCaminoEncontrado7),
    %Adyacente arriba derecha
    PosAdyArribaDerecha is Pos-NumOfColumns+1,
    adyacenteCamino(Grid, ElemInPos, Pos,PosAdyArribaDerecha,filaArriba,NumOfColumns,NumOfRows,ListaVisitados,CaminoEncontrado7,ValorCaminoEncontrado7,CaminoEncontrado8,ValorCaminoEncontrado8).

/**
 * mayorCaminoGrilla(+Grid, +GridAuxiliar, +IndiceElemento, +NumOfColumns, +NumOfRows, -MayorCamino, -MayorCaminoSumatoria)
 * GridAuxiliar una copia de la grilla, la cual iremos reduciendo a medida que hagamos las llamadas recursivas, para no alterar la grilla original,
 * de esta forma recorriendola indice por indice
 * MayorCamino es una lista que contiene los elementos del mayor camino de todos los analizados
 * MayorCaminoSumatoria es la sumatoria resultante de dicho camino
 * 
 **/ 
    mayorCaminoGrilla(Grid, [X|GrillaRestante], Indice, NumOfColumns,NumOfRows, MayorCamino, MayorCaminoSumatoria) :-
        IndiceAux is Indice + 1,
        mayorCaminoGrilla(Grid, GrillaRestante, IndiceAux,NumOfColumns,NumOfRows, MayorCaminoAux, ValorMayorCaminoAux),
        buscarCamino(Grid, X, Indice,NumOfColumns,NumOfRows,[Indice], [Indice], 0, MayorCaminoPosible, ValorCaminoPosible),
        (   ValorCaminoPosible >= ValorMayorCaminoAux,
            MayorCamino = MayorCaminoPosible,
            MayorCaminoSumatoria = ValorCaminoPosible
        ;
            MayorCamino = MayorCaminoAux,
            MayorCaminoSumatoria = ValorMayorCaminoAux
        ).
    mayorCaminoGrilla(Grid, [X], Indice,NumOfColumns,NumOfRows, MayorCamino, ValorMayorCamino) :-
        buscarCamino(Grid, X, Indice,NumOfColumns,NumOfRows, [Indice], [Indice], 0, MayorCamino, ValorMayorCamino).
    
/**
 * pasarIndiceAPath(+Indice, +NumOfColumns, -IndicePath)
 * Indice es la posición de nuestro elemento
 * Indice path es la posición de nuestro elemento pasada al formato adoptado por el Path. Por ejemplo, el elemento en la posicion 4, pasará a ser el [0,4]
 **/ 
pasarIndiceAPath(Indice,NumOfColumns,IndicePath):-IndiceColumna is  Indice mod NumOfColumns, IndiceFila is Indice // NumOfColumns,IndicePath=[IndiceFila,IndiceColumna]. 

/**
 * convertirCaminoAPath(+Camino, +NumOfColumns, -Path)
 * Camino es la lista que contiene nuestro camino
 * Path una lista de listas que contiene todas las posiciones de los elementos de nuestro camino con el formato [Fila, Columna]
 **/ 
convertirCaminoAPath([X|Sublist],NumOfColumns,Path):-convertirCaminoAPath(Sublist,NumOfColumns,PathRecursivo),pasarIndiceAPath(X,NumOfColumns,XaPath),Path=[XaPath|PathRecursivo].
convertirCaminoAPath([X],NumOfColumns,[XaPath]):-pasarIndiceAPath(X,NumOfColumns,XaPath).


/**
 * ayudaMovidaMaxima(+Grid, +NumOfColumns, +NumOfRows, -Path)
 * Path es nuestro camino convertido al formato adoptado para el Path
 **/ 
ayudaMovidaMaxima(Grid,NumOfColumns,NumOfRows,Path):-mayorCaminoGrilla(Grid,Grid,0,NumOfColumns,NumOfRows,MayorCamino,_ValorMayorCamino),reverse(MayorCamino,MayorCaminoEnOrden),convertirCaminoAPath(MayorCaminoEnOrden,NumOfColumns,Path).

/**
 * adyacenteCaminoParaAdyacentesIguales(+Grid, +Elemento, +PosicionElemento, +PosicionAdyacente, +Condicion, +NumOfColumns, +NumOfRows, +CaminoActual, +CaminoMaximo, +ValorCaminoMaximo, -CaminoEncontrado, -ValorCaminoEncontrado)
 * La variable Condicion es aquella donde le pasaremos el predicado para verificar en el caso que corresponda la condicion definida por ellos.
 * Condicion puede ser los siguientes predicados: mismaFila,FilaArriba,FilaAbajo.
 * CaminoActual es una lista que contiene el camino que esta siendo analizado actualmente
 * CaminoMaximo contiene el camino con la sumatoria más grande encontrado hasta ahora
 * ValorCaminoMax contiene dicha sumatoria
 * CaminoEncontrado es el camino resultante que devolveremos cuando terminemos de recorrer todos los posibles caminos
 * ValorCaminoEncontrado es el valor de dicho camino
 * 
 **/ 
adyacenteCaminoParaMAdyacentesIguales(Grid, ElemInPos, Pos,PosAdy,Condicion, NumOfColumns,NumOfRows,CaminoActual, CaminoMaximo, ValorCaminoMax, CaminoEncontrado, ValorCaminoEncontrado) :-
    PosAdy>=0,
    PosAdy<(NumOfColumns*NumOfRows),
    not(member(PosAdy, CaminoActual)),
    nth0(PosAdy, Grid, Achequear),
    cumpleCondicionesCamino(Achequear,ElemInPos,CaminoActual),call(Condicion,Pos,PosAdy),
    max_list(Grid, MaximoElemento),
    calcularSuma(Grid, [PosAdy|CaminoActual], ResSuma),
    ResSuma=<MaximoElemento,
    (
        (
            ResSuma >= ValorCaminoMax,
            cumpleCondicionMAdyacentesIguales(Grid,NumOfColumns,NumOfRows,[PosAdy|CaminoActual],ResSuma),
            buscarCaminoMAdyacentesIguales(Grid, Achequear, PosAdy,NumOfColumns,NumOfRows, [PosAdy|CaminoActual], [PosAdy|CaminoActual], ResSuma, CaminoEncontrado, ValorCaminoEncontrado)
        );
        (
            buscarCaminoMAdyacentesIguales(Grid, Achequear, PosAdy,NumOfColumns,NumOfRows, [PosAdy|CaminoActual], CaminoMaximo, ValorCaminoMax, CaminoEncontrado, ValorCaminoEncontrado)
        )
    ).
adyacenteCaminoParaMAdyacentesIguales(_, _ElemInPos, _Pos,_PosAdy,_Condicion, _NumOfColumns,_NumOfRows, _CaminoActual,CaminoMaximo,ValorCaminoMaximo,CaminoMaximo,ValorCaminoMaximo).

/**
 * buscarCaminoMAdyacentesIguales(+Grid, +GridAuxiliar, +PosicionElemento, +NumOfColumns, +NumOfRows, +MayorCamino, +MayorCaminoSumatoria)
 * GridAuxiliar una copia de la grilla, la cual iremos reduciendo a medida que hagamos las llamadas recursivas, para no alterar la grilla original
 * MayorCamino es una lista que contiene el mayor camino de todos los analizados según las condiciones para el booster MayorCaminoAdyacentesIguales
 * MayorCaminoSumatoria es la sumatoria resultante de dicho camino
 * 
 **/ 
buscarCaminoMAdyacentesIguales(Grid, ElemInPos, Pos,NumOfColumns,NumOfRows, CaminoActual, CaminoMaximo,ValorCaminoMaximo ,CaminoEncontrado8,ValorCaminoEncontrado8) :-
    %Adyacente abajo izquierda
    PosAdyAbajoIzquierda is Pos+NumOfColumns-1,
    adyacenteCaminoParaMAdyacentesIguales(Grid, ElemInPos, Pos,PosAdyAbajoIzquierda,filaAbajo,NumOfColumns,NumOfRows,CaminoActual,CaminoMaximo,ValorCaminoMaximo,CaminoEncontrado1,ValorCaminoEncontrado1),
    %Adyacente abajo
    PosAdyAbajo is Pos+NumOfColumns,
    adyacenteCaminoParaMAdyacentesIguales(Grid, ElemInPos, Pos,PosAdyAbajo,filaAbajo,NumOfColumns,NumOfRows,CaminoActual,CaminoEncontrado1,ValorCaminoEncontrado1,CaminoEncontrado2,ValorCaminoEncontrado2),
    %Adyacente abajo derecha
    PosAdyAbajoDerecha is Pos+NumOfColumns+1,
    adyacenteCaminoParaMAdyacentesIguales(Grid, ElemInPos, Pos,PosAdyAbajoDerecha,filaAbajo,NumOfColumns,NumOfRows,CaminoActual,CaminoEncontrado2,ValorCaminoEncontrado2,CaminoEncontrado3,ValorCaminoEncontrado3),
    %Adyacente izquierda
    PosAdyIzquierda is Pos-1,
    adyacenteCaminoParaMAdyacentesIguales(Grid, ElemInPos, Pos,PosAdyIzquierda,mismaFila,NumOfColumns,NumOfRows,CaminoActual,CaminoEncontrado3,ValorCaminoEncontrado3,CaminoEncontrado4,ValorCaminoEncontrado4),
    %Adyacente derecha
    PosAdyDerecha is Pos+1,
    adyacenteCaminoParaMAdyacentesIguales(Grid, ElemInPos, Pos,PosAdyDerecha,mismaFila,NumOfColumns,NumOfRows,CaminoActual,CaminoEncontrado4,ValorCaminoEncontrado4,CaminoEncontrado5,ValorCaminoEncontrado5),
    %Adyacente arriba izquierda
    PosAdyArribaIzquierda is Pos-NumOfColumns-1,
    adyacenteCaminoParaMAdyacentesIguales(Grid, ElemInPos, Pos,PosAdyArribaIzquierda,filaArriba,NumOfColumns,NumOfRows,CaminoActual,CaminoEncontrado5,ValorCaminoEncontrado5,CaminoEncontrado6,ValorCaminoEncontrado6),
    %Adyacente arriba
    PosAdyArriba is Pos-NumOfColumns,
    adyacenteCaminoParaMAdyacentesIguales(Grid, ElemInPos, Pos,PosAdyArriba,filaArriba,NumOfColumns,NumOfRows,CaminoActual,CaminoEncontrado6,ValorCaminoEncontrado6,CaminoEncontrado7,ValorCaminoEncontrado7),
    %Adyacente arriba derecha
    PosAdyArribaDerecha is Pos-NumOfColumns+1,
    adyacenteCaminoParaMAdyacentesIguales(Grid, ElemInPos, Pos,PosAdyArribaDerecha,filaArriba,NumOfColumns,NumOfRows,CaminoActual,CaminoEncontrado7,ValorCaminoEncontrado7,CaminoEncontrado8,ValorCaminoEncontrado8).



/**
 * mayorCaminoGrillaParaMAdyacentesIguales(+Grid, +GridAuxiliar, +IndiceElemento, +NumOfColumns, +NumOfRows, -MayorCamino, -MayorCaminoSumatoria)
 * GridAuxiliar una copia de la grilla, la cual iremos reduciendo a medida que hagamos las llamadas recursivas, para no alterar la grilla original,
 * de esta forma recorriendola indice por indice
 * MayorCamino es una lista que contiene los elementos del mayor camino de todos los analizados segun las condiciones para MayorAdyacentesIguales
 * MayorCaminoSumatoria es la sumatoria resultante de dicho camino
 * 
 **/ 
mayorCaminoGrillaParaMAdyacentesIguales(Grid, [X|GrillaRestante], Indice, NumOfColumns, NumOfRows, MayorCamino, MayorCaminoSumatoria) :-
    IndiceAux is Indice + 1,
    mayorCaminoGrillaParaMAdyacentesIguales(Grid, GrillaRestante, IndiceAux, NumOfColumns, NumOfRows, MayorCaminoGrilla, ValorMayorCaminoGrilla),
    buscarCaminoMAdyacentesIguales(Grid, X, Indice, NumOfColumns, NumOfRows, [Indice], [Indice], 0, MayorCaminoPos, ValorCaminoPos),
    (
        (  
            ValorCaminoPos >= ValorMayorCaminoGrilla,
            MayorCamino = MayorCaminoPos,
            MayorCaminoSumatoria = ValorCaminoPos
        )
        ;      
        (
                MayorCamino = MayorCaminoGrilla,
                MayorCaminoSumatoria = ValorMayorCaminoGrilla
        )
            
    ).

    mayorCaminoGrillaParaMAdyacentesIguales(Grid, [X], Indice,NumOfColumns,NumOfRows, MayorCamino, ValorMayorCamino) :-
    buscarCaminoMAdyacentesIguales(Grid, X, Indice,NumOfColumns,NumOfRows, [Indice], [Indice], 0, MayorCamino, ValorMayorCamino).


/**
 * ayudaMaximosIgualesAdyacentes(+Grid, +NumOfColumns, +NumOfRows, -Path) 
 * Path es nuestro camino convertido al formato adoptado para el Path
 **/ 
ayudaMaximosIgualesAdyacentes(Grid,NumOfColumns,NumOfRows,Path):-mayorCaminoGrillaParaMAdyacentesIguales(Grid,Grid,0,NumOfColumns,NumOfRows,MayorCamino,_ValorMayorCamino),reverse(MayorCamino,MayorCaminoEnOrden),convertirCaminoAPath(MayorCaminoEnOrden,NumOfColumns,Path).


/**
 * cumpleCondicionMAdyacentesIguales(+Grid,+NumOfColumns,+NumOfRows,+Path,+SumaCamino)
 * Verifica si el Path cumple las condiciones para el booster MayorAdyacentesIguales
 * Las condiciones explicadas de forma corta es que luego de borrar el PathEnOrden de la grilla, y aplicarle gravedad a la grilla con el PathEnOrden eliminado
 * la ultimaPosicion del PathEnOrden, en la cual se va a insertar la proxima potencia de 2 de SumaCamino, tenga algun adyacente con su mismo valor.
 * PathEnOrden es el Path pasado por parametro, luego de pasar por el reverse, ya que el Path se pasa invertido.
 * 
 * */
cumpleCondicionMAdyacentesIguales(Grid,NumOfColumns,NumOfRows,Path,SumaCamino):-calcularProximaPotencia(SumaCamino, 2,PotenciaPath),reverse(Path,PathEnOrden),generarGrillaAuxiliar(Grid, NumOfColumns, PathEnOrden,PotenciaPath,UltimoElementoPathIndice, ColumnaElementoPath, GridAuxSinGravedad), sacarUltimaPosicionColumna(NumOfColumns, ColumnaElementoPath, NumOfRows, UltimaPosColumna), actualizarPosicion(GridAuxSinGravedad, NumOfColumns, UltimoElementoPathIndice, UltimaPosColumna, 0, PosicionActualizada), gravedadGrillaAuxiliar(GridAuxSinGravedad,NumOfColumns,NumOfRows,GridAuxConGravedad),
    !,hayIgualEnGrillaAdyacente(GridAuxConGravedad,PosicionActualizada,NumOfColumns,PotenciaPath).


/**
 * hayIgualEnGrillaAdyacente(+Grid,+PosicionElemento,+NumOfColumns,+SumaPath)
 * Verifica si PosicionElemento cumple que en las posiciones adyacentes a el haya un elemento igual a SumaPath
 * */
%Adyacente abajo izquierda
hayIgualEnGrillaAdyacente(Grid,UltimoElementoPathIndice,NumOfColumns,SumaPath):-IndiceAux is UltimoElementoPathIndice+NumOfColumns-1,IndiceAux<40,filaAbajo(UltimoElementoPathIndice,IndiceAux),nth0(IndiceAux,Grid,Elemento),Elemento = SumaPath,!.
%Adyacente abajo
hayIgualEnGrillaAdyacente(Grid,UltimoElementoPathIndice,NumOfColumns,SumaPath):-IndiceAux is UltimoElementoPathIndice+NumOfColumns,IndiceAux<40,filaAbajo(UltimoElementoPathIndice,IndiceAux),nth0(IndiceAux,Grid,Elemento),Elemento = SumaPath,!.
%Adyacente abajo derecha
hayIgualEnGrillaAdyacente(Grid,UltimoElementoPathIndice,NumOfColumns,SumaPath):-IndiceAux is UltimoElementoPathIndice+NumOfColumns+1,IndiceAux<40,filaAbajo(UltimoElementoPathIndice,IndiceAux),nth0(IndiceAux,Grid,Elemento),Elemento = SumaPath,!.
%Adyacente izquierda
hayIgualEnGrillaAdyacente(Grid,UltimoElementoPathIndice,_NumOfColumns,SumaPath):-IndiceAux is UltimoElementoPathIndice-1,mismaFila(UltimoElementoPathIndice,IndiceAux),nth0(IndiceAux,Grid,Elemento),Elemento = SumaPath,!.
%Adyacente derecha
hayIgualEnGrillaAdyacente(Grid,UltimoElementoPathIndice,_NumOfColumns,SumaPath):-IndiceAux is UltimoElementoPathIndice+1,mismaFila(UltimoElementoPathIndice,IndiceAux),nth0(IndiceAux,Grid,Elemento),Elemento = SumaPath,!.
%Adyacente arriba izquierda
hayIgualEnGrillaAdyacente(Grid,UltimoElementoPathIndice,NumOfColumns,SumaPath):-IndiceAux is UltimoElementoPathIndice-NumOfColumns-1,IndiceAux>=0,filaArriba(UltimoElementoPathIndice,IndiceAux),nth0(IndiceAux,Grid,Elemento),Elemento = SumaPath,!.
%Adyacente arriba
hayIgualEnGrillaAdyacente(Grid,UltimoElementoPathIndice,NumOfColumns,SumaPath):-IndiceAux is UltimoElementoPathIndice-NumOfColumns,IndiceAux>=0,filaArriba(UltimoElementoPathIndice,IndiceAux),nth0(IndiceAux,Grid,Elemento),Elemento = SumaPath,!.
%Adyacente arriba derecha
hayIgualEnGrillaAdyacente(Grid,UltimoElementoPathIndice,NumOfColumns,SumaPath):-IndiceAux is UltimoElementoPathIndice-NumOfColumns+1,IndiceAux>=0,filaArriba(UltimoElementoPathIndice,IndiceAux),nth0(IndiceAux,Grid,Elemento),Elemento = SumaPath.

/**
 * generarGrillaAuxiliar(+Grid, +NumOfColumns, +Path,+SumaPath,-UltimoElementoPathIndice, -ColumnaElementoPath,-GridRes)
 * GridRes es Grid luego de eliminar todos los elementos de Path y poner en la posicion de su ultimo elemento SumaPath
 * UltimoElementoPathIndice es el indice del ultimo elemento insertado del Path, es decir, donde se inserto SumaPath
 * ColumnaElementoPath es la columna donde se encuentra el UltimoElementoPathIndice
 * */
generarGrillaAuxiliar(Grid, NumOfColumns, [X],SumaPath,X, ColumnaElementoPath, GridRes) :- reemplazarElementoI(Grid, X, SumaPath, GridRes), ColumnaElementoPath is X mod NumOfColumns.
generarGrillaAuxiliar(Grid, NumOfColumns, [X|Xs],SumaPath,UltimoElementoPathIndice, ColumnaElementoPath, GridRes) :- generarGrillaAuxiliar(Grid, NumOfColumns,Xs,SumaPath,UltimoElementoPathIndice, ColumnaElementoPath, GridAux) ,borrarElementoI(GridAux, X, GridRes).


/**
 * sacarDistintos0(+Lista,-Res)
 * Res es el resultado de eliminar todas las apariciones de 0 de la lista Lista
 * */
sacarDistintos0([X], [X]) :- X =\= 0.
sacarDistintos0([X], []) :- X =:= 0.
sacarDistintos0([X|Xs], [X|Res]) :- X =\= 0, sacarDistintos0(Xs, Res).
sacarDistintos0([X|Xs], Res) :- X =:= 0, sacarDistintos0(Xs,Res).
        
/**
 * poner0Adelante(+Lista,+Cantidad,-Res)
 * Res es el resultado de poner tantos 0's adelante del primer elemento segun Cantidad
 * */        
poner0Adelante(Lista,0,Lista).
poner0Adelante(Lista,Cantidad,[0|Res]):-CantidadAux is Cantidad-1,CantidadAux>=0,poner0Adelante(Lista,CantidadAux,Res). 
        

/**
* ordenarColumnasAuxiliar(+Grid,+Contador,+NumOfColumns,-Res)
* Res son las columnas de las grillas, con la gravedad aplicada, que estan concatenadas de la siguiente manera
* [ElementosColumna0..,ElementosColumna1..,ElementosColumna2..,ElementosColumna3..,ElementosColumna4..]
* */
ordenarColumnasAuxiliar(Grid, Contador,NumOfColumns, Res) :- Contador=:=(NumOfColumns-1),sacarColumnaI(Grid, 0, Contador, NumOfColumns, Columna), sacarDistintos0(Columna, ResAux2), length(Columna,CantElementos1),
        length(ResAux2,CantElementos2), Cantidad0 is CantElementos1-CantElementos2, poner0Adelante(ResAux2, Cantidad0, Res).
ordenarColumnasAuxiliar(Grid, Contador,NumOfColumns, Res) :- ContadorAux is Contador+1,ContadorAux<5,ordenarColumnasAuxiliar(Grid,ContadorAux,NumOfColumns,ColumnasOrdenadas),sacarColumnaI(Grid, 0, Contador,5, Columna), sacarDistintos0(Columna, ResAux2), length(Columna,CantElementos1),
        length(ResAux2,CantElementos2), Cantidad0 is CantElementos1-CantElementos2, poner0Adelante(ResAux2, Cantidad0, ResAux3),append(ResAux3,ColumnasOrdenadas,Res).


/**
* gravedadGrillaAuxiliar(+Grid,+NumOfColumns,+NumOfRows,-Res)
* Res es Grid luego de aplicarle la gravedad a toda la grilla, es decir, los elementos se desplazan x cuadrados hacia abajo, hasta que quede correctamente ordenada
* en una sola llamada.
* */      
gravedadGrillaAuxiliar(Grid,NumOfColumns,NumOfRows,Res):- ordenarColumnasAuxiliar(Grid,0,NumOfColumns, ResAux),armarGrilla(ResAux,0,NumOfRows,Res).

/**
* sacarUltimaPosicionColumna(+NumOfColumns,+Columna,+NumOfRows,-Res)
* Res es el indice de la ultima posicion de la columna Columna
* */  
sacarUltimaPosicionColumna(NumOfColumns, Columna, NumOfRows, Res) :- Res is ((NumOfColumns*(NumOfRows-1))+Columna).


/**
* actualizarPosicion(+Grid,+NumOfColumns,+PosicionAnterior,+UltimaPosicionColumna,+Cont,-Res)
* Res es el indice de UltimaPosicionColumna desplazado segun la cantidad de 0's abajo suyo, en su columna.
* */  
actualizarPosicion(_Grid, _NumOfColumns, PosicionAnterior, UltimaPosicionColumna, 0, Res) :- PosicionAnterior =:= UltimaPosicionColumna, Res is UltimaPosicionColumna.
actualizarPosicion(_Grid, NumOfColumns, PosicionAnterior, UltimaPosicionColumna, Cont, Res) :- PosicionAnterior =:= UltimaPosicionColumna, Res is UltimaPosicionColumna+(Cont*NumOfColumns).
actualizarPosicion(Grid, NumOfColumns, PosicionAnterior, UltimaPosicionColumna, Cont, Res) :- PosicionAnterior < UltimaPosicionColumna, nth0(UltimaPosicionColumna, Grid, ElementoUltimaPos), ElementoUltimaPos \= 0, UltimaPosicionAux is UltimaPosicionColumna-NumOfColumns, actualizarPosicion(Grid, NumOfColumns, PosicionAnterior, UltimaPosicionAux, Cont, Res).
actualizarPosicion(Grid, NumOfColumns, PosicionAnterior, UltimaPosicionColumna, Cont, Res) :- PosicionAnterior < UltimaPosicionColumna, nth0(UltimaPosicionColumna, Grid, ElementoUltimaPos), ElementoUltimaPos =:= 0,  ContAux is Cont+1, UltimaPosicionAux is UltimaPosicionColumna-NumOfColumns, actualizarPosicion(Grid, NumOfColumns, PosicionAnterior, UltimaPosicionAux, ContAux, Res).