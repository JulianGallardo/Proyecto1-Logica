:- module(proylcc, 
	[  
		join/5,
        powerUp/4,
        ayudaMovidaMaxima/3
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
* ordenarColumnas(+Grid,+Contador,-Res)
 * Res es una lista con todas las columnas de Grid concatenadas empezando de la 4 hasta la 0.
 * 
 **/
ordenarColumnas(Grid, 4,NumOfColumns, Res) :- sacarColumnaI(Grid, 0, 4, NumOfColumns, Columna), reemplazarUltimo0(Columna,Res).
ordenarColumnas(Grid, Contador,NumOfColumns, Res) :- ContadorAux is Contador+1,ordenarColumnas(Grid,ContadorAux,NumOfColumns,ColumnasOrdenadas),sacarColumnaI(Grid, 0, Contador,5, Columna), reemplazarUltimo0(Columna,ColumnaProcesada),append(ColumnaProcesada,ColumnasOrdenadas,Res).


/**
 * armarGrilla(+ColumnasConcatenadas,+Cont,+NumOfRows,-GrillaOrdenada)
 * GrillaOrdenada es la grilla armada devuelta a partir de agarrar la lista con las columnas concatenadas.
 **/
armarGrilla(ColumnasConcatenadas,Contador, NumOfRows, FilaOrdenada):-Contador=:=NumOfRows,sacarColumnaI(ColumnasConcatenadas,0,NumOfRows,NumOfRows,FilaOrdenada).
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


%Empieza_el_booster


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
 * adyacentes(+Grid,+ElemInPos,+Pos,+Lista,+ListaAdyacentesFinal)
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
 * powerUp(+Grid,-RGrid)
 * RGrid es una lista con los pasos que hace la grilla para aplicar el powerUp
 * El numero 0 en las celdas reperesenta una celda vacia.
 **/ 
powerUp(Grid,NumOfColumns,NumOfRows,RGrids):-borrarGrupos(Grid,GridA),grillasConGravedad(GridA,[GridA],NumOfColumns,NumOfRows,RGrids).



/**
 * Intento Nuevos Boosters
 * */

buscarMayoresIndices(Lista,ListaIndices):-max_list(Lista,Mayor),findall(Indice,indice(Mayor,Lista,Indice),ListaIndices).

mismaPotencia(X,Y,_Base):-X =:= Y.

potenciaSiguiente(X, Y, Pow) :-
    Pow =\= 1,        % Asegura que Pow no sea 1 (evita la repetición infinita)
    PowX is Y * Pow,  % Calcula la potencia siguiente de X
    PowX =:= X.       % Comprueba si la potencia siguiente de Y es igual a X
    


% Regla para buscar el índice de un elemento en una lista
indice(Elemento, Lista, Indice) :-
    indice(Elemento, Lista, 0, Indice).

% Caso base: el elemento se encuentra al principio de la lista
indice(Elemento, [Elemento|_], Indice, Indice).

% Caso recursivo: el elemento no está al principio de la lista
indice(Elemento, [_|Resto], IndiceActual, Indice) :-
    IndiceSiguiente is IndiceActual + 1,
    indice(Elemento, Resto, IndiceSiguiente, Indice).

cumpleCondicionesCamino(Pos,PosSiguiente,ListaVisitados):-
    length(ListaVisitados,Long),Long<2,mismaPotencia(Pos,PosSiguiente,2).
cumpleCondicionesCamino(Pos,PosSiguiente,ListaVisitados):-length(ListaVisitados,Long),Long>=1,mismaPotencia(Pos,PosSiguiente,2).
cumpleCondicionesCamino(Pos,PosSiguiente,ListaVisitados):-length(ListaVisitados,Long),Long>=1,potenciaSiguiente(Pos,PosSiguiente,2).
                                

adyacenteCaminoArriba(Grid, ElemInPos, Pos, ListaVisitados, CaminoMaximo, ValorCaminoMax, CaminoEncontrado, ValorCaminoEncontrado) :-
    PosAdyacenteSuperior is Pos - 5,
    PosAdyacenteSuperior >= 0,
    not(member(PosAdyacenteSuperior, ListaVisitados)),
    nth0(PosAdyacenteSuperior, Grid, Achequear),
    cumpleCondicionesCamino(Achequear,ElemInPos,ListaVisitados),
    calcularSuma(Grid, [PosAdyacenteSuperior|ListaVisitados], ResSuma),
    (
        (
            ResSuma >= ValorCaminoMax,
            buscarCamino(Grid, Achequear, PosAdyacenteSuperior, [PosAdyacenteSuperior|ListaVisitados], [PosAdyacenteSuperior|ListaVisitados], ResSuma, CaminoEncontrado, ValorCaminoEncontrado)
        );
        (
            buscarCamino(Grid, Achequear, PosAdyacenteSuperior, [PosAdyacenteSuperior|ListaVisitados], CaminoMaximo, ValorCaminoMax, CaminoEncontrado, ValorCaminoEncontrado)
        )
    ).
adyacenteCaminoArriba(_Grid, _ElemInPos, _Pos,_ListaVisitados,CaminoMaximo,ValorCaminoMaximo,CaminoMaximo,ValorCaminoMaximo).

adyacenteCaminoAbajo(Grid, ElemInPos, Pos, ListaVisitados, CaminoMaximo, ValorCaminoMax, CaminoEncontrado, ValorCaminoEncontrado) :-
    PosAdyacenteInferior is Pos + 5,
    PosAdyacenteInferior<40,
    not(member(PosAdyacenteInferior, ListaVisitados)),
    nth0(PosAdyacenteInferior, Grid, Achequear),
    cumpleCondicionesCamino(Achequear,ElemInPos,ListaVisitados),
    calcularSuma(Grid, [PosAdyacenteInferior|ListaVisitados], ResSuma),
    (
        (
            ResSuma >= ValorCaminoMax,
            buscarCamino(Grid, Achequear, PosAdyacenteInferior, [PosAdyacenteInferior|ListaVisitados], [PosAdyacenteInferior|ListaVisitados], ResSuma, CaminoEncontrado, ValorCaminoEncontrado)
        );
        (
            buscarCamino(Grid, Achequear, PosAdyacenteInferior, [PosAdyacenteInferior|ListaVisitados], CaminoMaximo, ValorCaminoMax, CaminoEncontrado, ValorCaminoEncontrado)
        )
    ).
adyacenteCaminoAbajo(_Grid, _ElemInPos, _Pos,_ListaVisitados,CaminoMaximo,ValorCaminoMaximo,CaminoMaximo,ValorCaminoMaximo).

adyacenteCaminoIzquierda(Grid, ElemInPos, Pos, ListaVisitados, CaminoMaximo, ValorCaminoMax, CaminoEncontrado, ValorCaminoEncontrado) :-
    PosAdyacenteIzq is Pos - 1,
    PosAdyacenteIzq>=0,
    not(member(PosAdyacenteIzq, ListaVisitados)),
    nth0(PosAdyacenteIzq, Grid, Achequear),
    cumpleCondicionesCamino(Achequear,ElemInPos,ListaVisitados),mismaFila(Pos, PosAdyacenteIzq),
    calcularSuma(Grid, [PosAdyacenteIzq|ListaVisitados], ResSuma),
    (
        (
            ResSuma >= ValorCaminoMax,
            buscarCamino(Grid, Achequear, PosAdyacenteIzq, [PosAdyacenteIzq|ListaVisitados], [PosAdyacenteIzq|ListaVisitados], ResSuma, CaminoEncontrado, ValorCaminoEncontrado)
        );
        (
            buscarCamino(Grid, Achequear, PosAdyacenteIzq, [PosAdyacenteIzq|ListaVisitados], CaminoMaximo, ValorCaminoMax, CaminoEncontrado, ValorCaminoEncontrado)
        )
    ).
adyacenteCaminoIzquierda(_Grid, _ElemInPos, _Pos,_ListaVisitados,CaminoMaximo,ValorCaminoMaximo,CaminoMaximo,ValorCaminoMaximo).


adyacenteCaminoDerecha(Grid, ElemInPos, Pos, ListaVisitados, CaminoMaximo, ValorCaminoMax, CaminoEncontrado, ValorCaminoEncontrado) :-
    PosAdyacenteDer is Pos + 1,
    PosAdyacenteDer<40,
    not(member(PosAdyacenteDer, ListaVisitados)),
    nth0(PosAdyacenteDer, Grid, Achequear),
    cumpleCondicionesCamino(Achequear,ElemInPos,ListaVisitados),mismaFila(Pos, PosAdyacenteDer),
    calcularSuma(Grid, [PosAdyacenteDer|ListaVisitados], ResSuma),
    (
        (
            ResSuma >= ValorCaminoMax,
            buscarCamino(Grid, Achequear, PosAdyacenteDer, [PosAdyacenteDer|ListaVisitados], [PosAdyacenteDer|ListaVisitados], ResSuma, CaminoEncontrado, ValorCaminoEncontrado)
        );
        (
            buscarCamino(Grid, Achequear, PosAdyacenteDer, [PosAdyacenteDer|ListaVisitados], CaminoMaximo, ValorCaminoMax, CaminoEncontrado, ValorCaminoEncontrado)
        )
    ).
adyacenteCaminoDerecha(_Grid, _ElemInPos, _Pos,_ListaVisitados,CaminoMaximo,ValorCaminoMaximo,CaminoMaximo,ValorCaminoMaximo).


adyacenteCaminoIzquierdaAbajo(Grid, ElemInPos, Pos, ListaVisitados, CaminoMaximo, ValorCaminoMax, CaminoEncontrado, ValorCaminoEncontrado) :-
    PosAdyInferiorIzq is Pos+4,
    PosAdyInferiorIzq<40,
    not(member(PosAdyInferiorIzq, ListaVisitados)),
    nth0(PosAdyInferiorIzq, Grid, Achequear),
   cumpleCondicionesCamino(Achequear,ElemInPos,ListaVisitados),filaAbajo(Pos,PosAdyInferiorIzq),
    calcularSuma(Grid, [PosAdyInferiorIzq|ListaVisitados], ResSuma),
    (
        (
            ResSuma >= ValorCaminoMax,
            buscarCamino(Grid, Achequear, PosAdyInferiorIzq, [PosAdyInferiorIzq|ListaVisitados], [PosAdyInferiorIzq|ListaVisitados], ResSuma, CaminoEncontrado, ValorCaminoEncontrado)
        );
        (
            buscarCamino(Grid, Achequear, PosAdyInferiorIzq, [PosAdyInferiorIzq|ListaVisitados], CaminoMaximo, ValorCaminoMax, CaminoEncontrado, ValorCaminoEncontrado)
        )
    ).
adyacenteCaminoIzquierdaAbajo(_Grid, _ElemInPos, _Pos,_ListaVisitados,CaminoMaximo,ValorCaminoMaximo,CaminoMaximo,ValorCaminoMaximo).


adyacenteCaminoIzquierdaArriba(Grid, ElemInPos, Pos, ListaVisitados, CaminoMaximo, ValorCaminoMax, CaminoEncontrado, ValorCaminoEncontrado) :-
    PosAdySuperiorIzq is Pos-4,
    PosAdySuperiorIzq>=0,
    not(member(PosAdySuperiorIzq, ListaVisitados)),
    nth0(PosAdySuperiorIzq, Grid, Achequear),
    cumpleCondicionesCamino(Achequear,ElemInPos,ListaVisitados),filaArriba(Pos,PosAdySuperiorIzq),
    calcularSuma(Grid, [PosAdySuperiorIzq|ListaVisitados], ResSuma),
    (
        (
            ResSuma >= ValorCaminoMax,
            buscarCamino(Grid, Achequear, PosAdySuperiorIzq, [PosAdySuperiorIzq|ListaVisitados], [PosAdySuperiorIzq|ListaVisitados], ResSuma, CaminoEncontrado, ValorCaminoEncontrado)
        );
        (
            buscarCamino(Grid, Achequear, PosAdySuperiorIzq, [PosAdySuperiorIzq|ListaVisitados], CaminoMaximo, ValorCaminoMax, CaminoEncontrado, ValorCaminoEncontrado)
        )
    ).
adyacenteCaminoIzquierdaArriba(_Grid, _ElemInPos, _Pos,_ListaVisitados,CaminoMaximo,ValorCaminoMaximo,CaminoMaximo,ValorCaminoMaximo).

adyacenteCaminoDerechaArriba(Grid, ElemInPos, Pos, ListaVisitados, CaminoMaximo, ValorCaminoMax, CaminoEncontrado, ValorCaminoEncontrado) :-
    PosAdyInferiorDer is Pos-6,
    PosAdyInferiorDer>=0,
    not(member(PosAdyInferiorDer, ListaVisitados)),
    nth0(PosAdyInferiorDer, Grid, Achequear),
    cumpleCondicionesCamino(Achequear,ElemInPos,ListaVisitados),filaArriba(Pos,PosAdyInferiorDer),
    calcularSuma(Grid, [PosAdyInferiorDer|ListaVisitados], ResSuma),
    (
        (
            ResSuma >= ValorCaminoMax,
            buscarCamino(Grid, Achequear, PosAdyInferiorDer, [PosAdyInferiorDer|ListaVisitados], [PosAdyInferiorDer|ListaVisitados], ResSuma, CaminoEncontrado, ValorCaminoEncontrado)
        );
        (
            buscarCamino(Grid, Achequear, PosAdyInferiorDer, [PosAdyInferiorDer|ListaVisitados], CaminoMaximo, ValorCaminoMax, CaminoEncontrado, ValorCaminoEncontrado)
        )
    ).
adyacenteCaminoDerechaArriba(_Grid, _ElemInPos, _Pos,_ListaVisitados,CaminoMaximo,ValorCaminoMaximo,CaminoMaximo,ValorCaminoMaximo).

adyacenteCaminoDerechaAbajo(Grid, ElemInPos, Pos, ListaVisitados, CaminoMaximo, ValorCaminoMax, CaminoEncontrado, ValorCaminoEncontrado) :-
    PosAdySuperiorDer is Pos+6,
    PosAdySuperiorDer>=0,
    not(member(PosAdySuperiorDer, ListaVisitados)),
    nth0(PosAdySuperiorDer, Grid, Achequear),
    cumpleCondicionesCamino(Achequear,ElemInPos,ListaVisitados),filaAbajo(Pos,PosAdySuperiorDer),
    calcularSuma(Grid, [PosAdySuperiorDer|ListaVisitados], ResSuma),
    (
        (
            ResSuma >= ValorCaminoMax,
            buscarCamino(Grid, Achequear, PosAdySuperiorDer, [PosAdySuperiorDer|ListaVisitados], [PosAdySuperiorDer|ListaVisitados], ResSuma, CaminoEncontrado, ValorCaminoEncontrado)
        );
        (
            buscarCamino(Grid, Achequear, PosAdySuperiorDer, [PosAdySuperiorDer|ListaVisitados], CaminoMaximo, ValorCaminoMax, CaminoEncontrado, ValorCaminoEncontrado)
        )
    ).
adyacenteCaminoDerechaAbajo(_Grid, _ElemInPos, _Pos,_ListaVisitados,CaminoMaximo,ValorCaminoMaximo,CaminoMaximo,ValorCaminoMaximo).


buscarCamino(Grid, ElemInPos, Pos, ListaVisitados, CaminoMaximo,ValorCaminoMaximo ,CaminoEncontrado7,ValorCaminoEncontrado7) :-
    adyacenteCaminoDerechaAbajo(Grid, ElemInPos, Pos,ListaVisitados,CaminoMaximo,ValorCaminoMaximo,CaminoEncontrado,ValorCaminoEncontrado),adyacenteCaminoAbajo(Grid, ElemInPos, Pos,ListaVisitados,CaminoEncontrado,ValorCaminoEncontrado,CaminoEncontrado1,ValorCaminoEncontrado1),adyacenteCaminoIzquierdaAbajo(Grid, ElemInPos, Pos,ListaVisitados,CaminoEncontrado1,ValorCaminoEncontrado1,CaminoEncontrado2,ValorCaminoEncontrado2),
    adyacenteCaminoDerecha(Grid, ElemInPos, Pos,ListaVisitados,CaminoEncontrado2,ValorCaminoEncontrado2,CaminoEncontrado3,ValorCaminoEncontrado3), adyacenteCaminoIzquierda(Grid, ElemInPos, Pos,ListaVisitados,CaminoEncontrado3,ValorCaminoEncontrado3,CaminoEncontrado4,ValorCaminoEncontrado4),
    adyacenteCaminoDerechaArriba(Grid, ElemInPos, Pos,ListaVisitados,CaminoEncontrado4,ValorCaminoEncontrado4,CaminoEncontrado5,ValorCaminoEncontrado5),adyacenteCaminoArriba(Grid, ElemInPos, Pos,ListaVisitados,CaminoEncontrado5,ValorCaminoEncontrado5,CaminoEncontrado6,ValorCaminoEncontrado6),adyacenteCaminoIzquierdaArriba(Grid, ElemInPos, Pos,ListaVisitados,CaminoEncontrado6,ValorCaminoEncontrado6,CaminoEncontrado7,ValorCaminoEncontrado7).

mayorCaminoGrilla(Grid, [X|GrillaRestante], Indice, MayorCamino, MayorCaminoSumatoria) :-
    IndiceAux is Indice + 1,
    mayorCaminoGrilla(Grid, GrillaRestante, IndiceAux, MayorCaminoGrilla, ValorMayorCaminoGrilla),
    buscarCamino(Grid, X, Indice, [Indice], [Indice], 0, MayorCaminoPos, ValorCaminoPos),
    (   ValorCaminoPos >= ValorMayorCaminoGrilla,
        MayorCamino = MayorCaminoPos,
        MayorCaminoSumatoria = ValorCaminoPos
    ;
        MayorCamino = MayorCaminoGrilla,
        MayorCaminoSumatoria = ValorMayorCaminoGrilla
    ).
mayorCaminoGrilla(Grid, [X], Indice, MayorCamino, ValorMayorCamino) :-
    buscarCamino(Grid, X, Indice, [Indice], [Indice], 0, MayorCamino, ValorMayorCamino).


pasarIndiceAPath(X,NumOfColumns,IndicePath):-IndiceColumna is  X mod NumOfColumns, IndiceFila is X // NumOfColumns,IndicePath=[IndiceFila,IndiceColumna]. 

convertirCaminoAPath([X|Sublist],NumOfColumns,Path):-convertirCaminoAPath(Sublist,NumOfColumns,PathRecursivo),pasarIndiceAPath(X,NumOfColumns,XaPath),Path=[XaPath|PathRecursivo].
convertirCaminoAPath([X],NumOfColumns,[XaPath]):-pasarIndiceAPath(X,NumOfColumns,XaPath).


ayudaMovidaMaxima(Grid,NumOfColumns,Path):-mayorCaminoGrilla(Grid,Grid,0,MayorCamino,_ValorMayorCamino),reverse(MayorCamino,MayorCaminoEnOrden),convertirCaminoAPath(MayorCaminoEnOrden,NumOfColumns,Path).


