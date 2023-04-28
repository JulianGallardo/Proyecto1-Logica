import React from "react";
import Square from "./Square";
import imagenPowerUp from "./Assets/PowerUp.png"

function Button({onClickEvent}){
    return(
    <div className="botonPowerUp">
        <img src={imagenPowerUp} alt="Imagen Power-Up" style={{width: '10%' } } />
       <Square
            value={"Colapsar Iguales"}
            onClick={onClickEvent}
       />  
    </div>
    );
}

export default Button;