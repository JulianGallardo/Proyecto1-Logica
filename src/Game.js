import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import { joinResult} from './util';
import Marker from './marker';
import Button from './Button';
import imagenColapsarIguales from "./Assets/PowerUp.png"
import imagenAyudaMovidaMaxima from "./Assets/ayudaMovidaMaxima.png"
import imagenMaximosIgualesAdyacentes from "./Assets/ayudaMaximosIgualesAdyacentes.png"

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [numOfColumns, setNumOfColumns] = useState(null);
  const [numOfRows,setNumOfRows]=useState(null);
  const [score, setScore] = useState(0);
  const [path, setPath] = useState([]);
  const [waiting, setWaiting] = useState(false);
  const [newBlockValue, setValue] =useState(1);
  const [btn_ColapsarIguales,setEstadoBtnColapsarIguales]=useState(false);
  const [btn_AyudaMovidaMaxima,setEstadoBtnAyudaMovidaMaxima]=useState(false);
  const [btn_AyudaMaximosIgualesAdyacentes,setEstadoBtnAyudaMaximosIgualesAdyacentes]=useState(false);

  
  useEffect(() => {
    // This is executed just once, after the first render.
    PengineClient.init(onServerReady);
  }, []);

  /**
   * Called when the server was successfully initialized
   */
  function onServerReady(instance) {
    pengine = instance;
    const queryS = 'init(Grid, NumOfColumns,NumOfRows)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setNumOfColumns(response['NumOfColumns']);
        setNumOfRows(response['NumOfRows'])
      }
    });
  }

  /**
   * Called while the user is drawing a path in the grid, each time the path changes.
   */
  function onPathChange(newPath) {
    // No effect if waiting.
    if (waiting) {
      return;
    }
    actualizarValorNuevoBloque(newPath);
    setPath(newPath);
  }

/**
 * Actualiza el valor del nuevo bloque a generarse segun el camino.
 */
  function actualizarValorNuevoBloque(newPath){ 
    if(newPath.length!==0){
      var newBlockScore=joinResult(newPath, grid, numOfColumns);
      setValue(newBlockScore);
    }
  }





  /**
   * Called when the user press the AyudaMovidaMaxima button.
   */
  function AyudaMovidaMaxima(){
    /*
    Build Prolog query, which will be like:
    ayudaMovidaMaxima([
          64,4,64,32,16,
          64,8,16,2,32,
          2,4,64,64,2,
          2,4,32,16,4,
          16,4,16,16,16,
          16,64,2,32,32,
          64,2,64,32,64,
          32,2,64,32,4
          ],
          NumOfColumns,Path
        ).
    */
          
        const gridS = JSON.stringify(grid);
        const queryS = "ayudaMovidaMaxima(" + gridS + ","+numOfColumns+","+numOfRows+", Path)";
        if(!btn_ColapsarIguales){
        setWaiting(true);
        setEstadoBtnAyudaMovidaMaxima(true);
        
        pengine.query(queryS, (success, response) => {
          if (success && response['Path'].length>1) {
            onPathChange(response['Path']);
            setWaiting(false);
            setEstadoBtnAyudaMovidaMaxima(false);
          } else {
            setWaiting(false);
            setEstadoBtnAyudaMovidaMaxima(false);
          }
         
        }); 
      }

  }

  function AyudaMaximosIgualesAdyacentes(){
    /*
    Build Prolog query, which will be like:
    ayudaMaximosIgualesAdyacentes([
          64,4,64,32,16,
          64,8,16,2,32,
          2,4,64,64,2,
          2,4,32,16,4,
          16,4,16,16,16,
          16,64,2,32,32,
          64,2,64,32,64,
          32,2,64,32,4
          ],
          NumOfColumns,NumOfRows,Path
        ).
    */
   
        const gridS = JSON.stringify(grid);
        const queryS = "ayudaMaximosIgualesAdyacentes(" + gridS + ","+numOfColumns+","+numOfRows+", Path)";
        if(!btn_ColapsarIguales){
          setWaiting(true);
          setEstadoBtnAyudaMaximosIgualesAdyacentes(true);
          pengine.query(queryS, (success, response) => {
            
              if (success && response['Path'].length>1) {
                onPathChange(response['Path']);
                setWaiting(false);
                setEstadoBtnAyudaMaximosIgualesAdyacentes(false);
              } else {
                setWaiting(false);
                setEstadoBtnAyudaMaximosIgualesAdyacentes(false);
              }
            
          });
        }
    }







  /**
   * Called when the user press the ColapsarIguales button.
   */
  function booster(){
    /*
    Build Prolog query, which will be like:
    booster([
          64,4,64,32,16,
          64,8,16,2,32,
          2,4,64,64,2,
          2,4,32,16,4,
          16,4,16,16,16,
          16,64,2,32,32,
          64,2,64,32,64,
          32,2,64,32,4
          ],
          RGrids
        ).
    */
          
        const gridS = JSON.stringify(grid);
        const queryS = "powerUp(" + gridS + ","+numOfColumns+","+numOfRows+", RGrids)";
       
        setWaiting(true);
        
        
        pengine.query(queryS, (success, response) => {
          if (success && path.length===0) {
            setEstadoBtnColapsarIguales(true);
            animateEffect(response['RGrids']);
            setWaiting(false)
          } else {
            setWaiting(false);
            setEstadoBtnColapsarIguales(false);
          }
         
        });
        
        

  }


  /**
   * Called when the user finished drawing a path in the grid.
   */
  function onPathDone() {
    /*
    Build Prolog query, which will be like:
    join([
          64,4,64,32,16,
          64,8,16,2,32,
          2,4,64,64,2,
          2,4,32,16,4,
          16,4,16,16,16,
          16,64,2,32,32,
          64,2,64,32,64,
          32,2,64,32,4
          ], 
          5, 
          [[2, 0], [3, 0], [4, 1], [3, 1], [2, 1], [1, 1], [1, 2], [0, 3]],
          RGrids
        ).
    */
    const gridS = JSON.stringify(grid);
    const pathS = JSON.stringify(path);
    const queryS = "join(" + gridS + "," + numOfColumns + ","+numOfRows + "," + pathS + ", RGrids)";
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        
        setScore(score + joinResult(path, grid, numOfColumns));
        setPath([]);
        animateEffect(response['RGrids']);
        
      } else {
        setWaiting(false);
        
      }

    });
  }

  /**
   * Displays each grid of the sequence as the current grid in 1sec intervals.
   * @param {number[][]} rGrids a sequence of grids.
   */
  function animateEffect(rGrids) {
    setGrid(rGrids[0]);
    const restRGrids = rGrids.slice(1);
    if (restRGrids.length > 0) {
      setTimeout(() => {
        animateEffect(restRGrids);
      }, 500);
    } else {
      setEstadoBtnColapsarIguales(false);
      setWaiting(false);
    }
  }

  if (grid === null) {
    return null;
  }
  return (
    <div className="game">
      <div className='header'>
        <Marker
          score={score}
          path={path}
          newBlockValue={newBlockValue}
        />
      </div>
      <Board
        grid={grid}
        numOfColumns={numOfColumns}
        path={path}
        onPathChange={onPathChange}
        onDone={onPathDone}
      />
        <div className="ContenedorBotones">
        <Button
          className={"ColapsarIguales"}
          Text={"Colapsar Iguales"}
          Estado={btn_ColapsarIguales}
          rutaImagen={imagenColapsarIguales}
          onClickEvent={booster}
        />
        <Button
          className={"AyudaMovidaMaxima"}
          Text={"Ayuda Movida Maxima"}
          rutaImagen={imagenAyudaMovidaMaxima}
          Estado={btn_AyudaMovidaMaxima}
          onClickEvent={AyudaMovidaMaxima}
        />
        <Button
          className={"AyudaMaximosIguales"}
          Text={"Ayuda máximos iguales adyacentes"}
          Estado={btn_AyudaMaximosIgualesAdyacentes}
          rutaImagen={imagenMaximosIgualesAdyacentes}
          onClickEvent={AyudaMaximosIgualesAdyacentes}
        />
      </div>
    </div>

  );
}

export default Game;