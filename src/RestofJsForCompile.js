

function handleInputChange(event) {
	


    var parentDivId = event.target.id.replace("input_", "");
  
    switch (map1.get(parentDivId).type) {
      case "integer":
          if(event.target.value==""){
            values.set(map1.get(parentDivId).variable,0);
  
          }else
          
        values.set(map1.get(parentDivId).variable, parseInt(event.target.value));
        break;
      case "boolean":
            if(event.target.value==""){
                values.set(map1.get(parentDivId).variable,false);
          }else
        values.set(map1.get(parentDivId).variable, event.target.checked);
        break;
      case "string":
           if(event.target.value==""){
            values.set(map1.get(parentDivId).variable,"");
          }else
        values.set(map1.get(parentDivId).variable, event.target.value);
        break;
    }
  
  
      update();
          
      
  
  
  
  }
  
  
  
  function update(){
      let changed = true;
    while (changed) {
      changed = false;
      for (let [key, value] of map1) {

    
        if (map1.get(key).isEnabled && eval(map1.get(key).isEnabled)) {

            document.getElementById(key).style.visibility = "visible";
          if (value.kind == "computable") {
            var newValue=eval(value.expr);
            if(value.type=='integer'){
            if(!isNaN(newValue)){
                if(!isFinite(newValue))newValue=0;
              Math.ceil(newValue);
            }
            
            if(isNaN(newValue))newValue=0;
            }
            let curretnValueInBox=document.getElementById("input_" + key).value;


            if (curretnValueInBox!=newValue.toString()) {
              changed = true;

              
              values.set(value.variable, newValue);
  
              document.getElementById("input_" + key).value = newValue;
              if(value.type=='boolean')
                  document.getElementById("input_" + key).checked=newValue;

                
            } 
          } 
        }else if (map1.get(key).isEnabled && !eval(map1.get(key).isEnabled)){
                
            document.getElementById(key).style.visibility = "hidden";
            }
      }
    } 
  
  
  }
  
  
  document.addEventListener("DOMContentLoaded", () => {
    const inputs = document.querySelectorAll("input");
    inputs.forEach((input) => {
      if (!input.disabled) {
        
        input.addEventListener("input", handleInputChange);
      }
    });
  });
  
  
  for (let [key, value] of map1) {
    if (value.kind == "computable" || value.kind == "answerable") {
      switch (value.type) {
        case "integer":
          values.set(value.variable, 0);
          break;
        case "string":
          values.set(value.variable, "");
          break;
  
        case "boolean":
          values.set(value.variable, false);
          break;
      }
    }
  }
  
  update();
