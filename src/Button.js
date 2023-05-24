import React from "react";
import Square from "./Square";




function Button({onClickEvent,Estado,Text,rutaImagen}){
    return(
    <div className={"btn_"+Text.split(' ').join('')}>
        <img src={rutaImagen} alt={"Imagen "+Text} style={{width: '10%' } } />
       <Square
            value={Estado ?"Procesando...":Text }
            onClick={Estado ? null:onClickEvent}
       />  
    </div>
    );
}

export default Button;