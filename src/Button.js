import React from "react";
import Square from "./Square";
import imagenPowerUp from "./Assets/PowerUp.png"



function Button({onClickEvent,Estado}){
    return(
    <div className="botonPowerUp">
        <img src={imagenPowerUp} alt="Imagen Power-Up" style={{width: '10%' } } />
       <Square
            value={Estado ?"Procesando...":"Colapsar Iguales" }
            onClick={Estado ? null:onClickEvent}
       />  
    </div>
    );
}

export default Button;