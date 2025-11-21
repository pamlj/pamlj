var fun=require('./functions');

const events = {

 
 aim_changed: function(ui) {
 
  fixclusters(ui, ui.clusterpars.value());  
   
 },
 find_changed: function(ui) {
 
  fixclusters(ui, ui.clusterpars.value());  
   
 },
 

code_changed: function(ui) {

    console.log("code changed");

    var clusters = ui.clusterpars.value();
    var code = ui.code.value();

    // keep only non-comment lines
    code = code.split("\n");
    code = code.filter(str => !/^#/.test(str));

    // take the first line containing "~" as the model formula
    var str = code.filter(str => str.includes("~"));
    str = str[0];

    // --- RANDOM CLUSTERS PART -----------------------------------
    // capture what comes after "|" up to space, "+", "(" or ")"
    // e.g. "| cluster1/cluster2)"  ->  "cluster1/cluster2"
    //      "| cluster1:cluster2)"  ->  "cluster1:cluster2"
    //      "| cluster2)"           ->  "cluster2"
    var regex = /\|\s*([^\s\+\)\(]+)/g;
    var rawGroups = [];
    var match;

    while ((match = regex.exec(str)) !== null) {
        rawGroups.push(match[1]);
    }

    // split on "/" and ":" to get primitive cluster variables
    // "cluster1/cluster2"   -> ["cluster1", "cluster2"]
    // "cluster1:cluster2"   -> ["cluster1", "cluster2"]
    // "cluster2"            -> ["cluster2"]
    var uniq = [];
    rawGroups.forEach(g => {
        g.split(/[/:]/).forEach(part => {
            part = part.trim();
            if (part && !uniq.includes(part))
                uniq.push(part);
        });
    });

    if (uniq.length === 0) {
        ui.clusterpars.setValue([]);
        // you can return here if you want to skip the rest
        // return;
    }

    // preserve existing k,n where possible
    var newclusters = uniq.map(element => {
        if (clusters.length > 0) {
            var found = clusters.find(item => item.name === element);
            if (found !== undefined)
                return found;
        }
        return { name: element, k: 0, n: 0 };
    });

    fixclusters(ui, newclusters);
    // --- END RANDOM CLUSTERS PART -------------------------------
    // --- FIXED EFFECT VARIABLES PART (unchanged except for indent) ---
    regex = /\*\s*(\w+)/g;
    var vars = [];
    while ((match = regex.exec(str)) !== null) {
        vars.push(match[1]);
    }
    vars = [...new Set(vars)];
    vars = vars.filter(element => element !== "1");

    var vartype = ui.var_type.value();
    var newvartype = vars.map(item => {
        var found = vartype.filter(element => element.name === item);
        if (found.length === 0)
            return { name: item, type: "continuous", levels: "---" };
        else
            return found[0];
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
   
    return;
    // i am not sure this is usefull, so let it out for now
    console.log("Clusterpars changed");
    var clusters =  utils.clone(ui.clusterpars.value(), []);  
 
    if (ui.aim.value() === "power") 
         return;
         
    var find =  ui.find.value();  
     
    var found = false;
    if (find === "k") {
        var clusters = clusters.map(item => {
            console.log(item.k);
            if (item.k == '0' ) {
                   item.k = "!";
                   found= true;
            }
            return(item)
        });
    }
    if (find === "n") {
        var clusters = clusters.map(item => {
            console.log(item.n);
            if (item.n == "0") {
                   item.n = "!";
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
  
 
   if (clusters.length === 0) return;

   var aim=ui.aim.value();
   var find=ui.find.value();
 
   let newclusters ;
   let val1;
   let val2;
   if (aim  === "n") {
     if (find === "k" ) {
      newclusters = clusters.map(e => {
       val1 = (!isFinite(Number(e.n)) || e.n === null || e.n === undefined || e.n === '') ? "0" : e.n;
       val2 = (!isFinite(Number(e.k)) || Number(e.k)  == 0 ) ?  "?" : e.k ;
        return({name: e.name, n : val1, k: val2});
      });
     }
     if (find == "n" ) {
      newclusters = clusters.map(e => {
        val1 = (!isFinite(Number(e.n)) || Number(e.n)  == 0  ) ?  "?" : e.n ;
        val2 = (!isFinite(Number(e.k)) || e.k === null || e.k === undefined || e.k === '') ? "0" : e.k;
        return({name: e.name, n : val1, k: val2});
      });
     }
   }  else {
        newclusters = clusters.map(e => {
         const nStr = (e.n !== undefined && e.n !== null) ? String(e.n).trim() : "";
         val1 = (nStr.trim() !== '' && isFinite(Number(e.n))) ? e.n:  "0" 
         const kStr = (e.k !== undefined && e.k !== null) ? String(e.k).trim() : "";
         val2 = (kStr.trim() !== '' && isFinite(Number(e.k))) ? e.k:  "0" 
         return({name: e.name, n : val1, k: val2});
        });
   }
   ui.clusterpars.setValue(newclusters);

}