import React from 'react';
import Square from './Square';
function Marker({score,path,newBlockValue}){
    if(path.length<=1)
        return(
                <div className="score" >
                    Score<br/>
                    {score}
                </div>
        );
    else{
        return(
                <div className="squareToPut"> 
                    <Square 
                        value={newBlockValue}
                     />
                </div>
        );
    }
}
export default Marker;
