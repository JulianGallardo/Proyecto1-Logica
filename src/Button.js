import React from "react";
import Square from "./Square";




function Button({onClickEvent,Estado,Text,rutaImagen,className,Disabled}){
    return(
    <div className={"btn_"+className}>
        <img className="ImagenBtn" src={rutaImagen} alt={"Imagen "+Text} />
       <Square
            value={Estado ?"Procesando...":Text }
            onClick={Estado||Disabled ? null:onClickEvent}
       />  
    </div>
    );
}

export default Button;