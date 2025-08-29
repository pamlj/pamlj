var fun=require('./functions');

const events = {

 aim_changed: function(ui) {
 
//  fixclusters(ui, ui.clusterpars.value());  
   
 },
 find_changed: function(ui) {
 
//  fixclusters(ui, ui.clusterpars.value());  
   
 },
 
 code_changed: function(ui) {
   
   console.log("code changed")
   var clusters= ui.clusterpars.value();
   var code= ui.code.value();
   code = code.split("\n");
   code = code.filter(str => !/^#/.test(str));
   var str = code.filter(str =>str.includes("~"));
   str = str[0]
  
   var regex = /\|\s*(\w+)/g;
   let matches = [];
   let match;
   while ((match = regex.exec(str)) !== null) {
        matches.push(match[1]); 
   }
   matches = [...new Set(matches)];
   if (matches.length === 0) {
     ui.clusterpars.setValue([]);
   };
   
   var newclusters= matches.map(element => {
       if (clusters.length > 0) {
          match = clusters.find(item => item.name === element);
          if (match !== undefined)
               return(match);
        }
       return({name: element, k: 0 , n: 0})
   });
 
        fixclusters(ui,newclusters);

        regex = /\*\s*(\w+)/g;
    var vars=[];
        while ((match = regex.exec(str)) !== null) {
              vars.push(match[1]); 
          }
        vars = [...new Set(vars)];          
        
//        if (!vars.some(element => element === "1"))
//            return;
        vars = vars.filter(element => element !== "1");

    var vartype = ui.var_type.value();        
    let found;
    var newvartype= vars.map(item => {
                      found = vartype.filter(element => element.name === item)
                      if (found.length === 0)
                            return({name: item, type: "continuous", levels: "---"})
                      else 
                            return(found[0])  
                   });
        ui.var_type.setValue(newvartype);               
 },
 
 var_type_changed: function(ui) {
   
    var vartype =  utils.clone(ui.var_type.value(), []);  

    var found = false;
    var newvartype = vartype.map(item => {
    
      if (item.type === "categorical" && item.levels === "---") {
          item.levels = "?"
          found=true;
      }
      if (item.type === "continuous" && item.levels !== "---") {
          item.levels = "---"
          found=true;
      }
      return(item)
    });

     if (found) {
          ui.var_type.setValue(newvartype);
     }
  },
 
 clusterpars_changed:  function(ui) {
   
    var clusters =  utils.clone(ui.clusterpars.value(), []);  
    return;
    if (ui.aim.value() === "power") 
         return;
         
    var find =  ui.find.value();  
     
    var found = false;
    if (find === "k") {
        var clusters = clusters.map(item => {
            if (item.k !== "---") {
                   item.k = "---";
                   found= true;
            }
            return(item)
        });
    }
    if (find === "n") {
        var clusters = clusters.map(item => {
            if (item.n !== "---") {
                   item.n = "---";
                   found= true;
            }
            return(item)
        });
    }

     if (found) {
          ui.clusterpars.setValue(clusters);
     }
   
 },
  lav_diagram_changed:  function(ui) {
   
   console.log("diagram changed");
 }


};

module.exports = events;

var fixclusters = function(ui, clusters) {
  
   console.log("fixclusters");
   if (clusters.length === 0) return;

   var value=ui.aim.value();
   var find=ui.find.value();
 
   let newclusters ;
   let val1;
   let val2;
   if (value  === "n") {
     if (find === "k" ) {
      newclusters = clusters.map(e => {
        val1 = (e.n  === "---") ?  "?" : e.n
        return({name: e.name, n : val1, k: "---"});
      });
     }
     if (find == "n" ) {
      newclusters = clusters.map(e => {
        val1 = (e.k  === "---") ?  "?" : e.k
        return({name: e.name, n : "---", k: val1});
      });
     }
   }  else {
        newclusters = clusters.map(e => {

        val1 = (e.k  === "---") ?  "?" : e.k
        val2 = (e.n  === "---") ?  "?" : e.n 
        return({name: e.name, k : val1, n: val2});
        });
   }
   ui.clusterpars.setValue(newclusters);

}