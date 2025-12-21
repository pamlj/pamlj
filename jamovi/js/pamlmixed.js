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
    var code     = ui.code.value();
    var vartype  = ui.var_type.value();

    // get model formula line
    var str = getFormulaLine(code);
    if (!str) {
        // nothing to do if no formula line
        ui.clusterpars.setValue([]);
        ui.var_type.setValue([]);
        return;
    }

    // --- RANDOM CLUSTERS PART -----------------------------------
    var newclusters = computeRandomClusters(str, clusters);
    ui.clusterpars.setValue(newclusters);
    if (newclusters.length > 0)
        fixclusters(ui, newclusters);   // keep your existing helper

    // --- FIXED EFFECT VARIABLES PART ----------------------------
    var newvartype = computeVarTypes(str, vartype);
    ui.var_type.setValue(newvartype);
},

 var_type_changed: function(ui) {
   
    console.log("var_type_changed");
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


// Get first non-comment line containing "~"
function getFormulaLine(code) {
    var lines = code.split("\n");
    // drop comment lines
    lines = lines.filter(str => !/^#/.test(str));
    // take first line with "~"
    var formulaLines = lines.filter(str => str.includes("~"));
    return formulaLines.length > 0 ? formulaLines[0] : "";
}


// Extract random cluster specs and preserve existing k,n when possible
function computeRandomClusters(str, clusters) {

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

    if (uniq.length === 0)
        return [];

    // preserve existing k,n where possible
    var newclusters = uniq.map(element => {
        if (clusters && clusters.length > 0) {
            var found = clusters.find(item => item.name === element);
            if (found !== undefined)
                return found;
        }
        return { name: element, k: 0, n: 0 };
    });

    return newclusters;
}


function computeVarTypes(str, vartype) {

    // 1. Remove random-effect parts: anything like (...|...)
    //    so "(1*1|cluster/school)" disappears entirely.
    var s = str.replace(/\([^()]*\|[^()]*\)/g, "");

    // 2. Work only on RHS (everything after "~")
    var tildeIndex = s.indexOf("~");
    if (tildeIndex === -1)
        return [];

    var rhs = s.slice(tildeIndex + 1);

    // 3. Scan RHS for word tokens and skip those followed by "*"
    var tokenRegex = /[A-Za-z_]\w*/g;
    var varsSet = new Set();
    var match;

    while ((match = tokenRegex.exec(rhs)) !== null) {
        var name = match[0];

        // Look ahead from end of this token to the next non-space char
        var i = match.index + name.length;
        while (i < rhs.length && /\s/.test(rhs[i]))
            i++;

        // If next non-space char is "*", treat this as a coefficient and skip
        if (rhs[i] === "*")
            continue;

        varsSet.add(name);
    }

    var vars = Array.from(varsSet);
    // 4. Preserve existing type info where possible
    var newvartype = vars.map(function(item) {
        var found = vartype.filter(function(element) {
            return element.name === item;
        });
        if (found.length === 0)
            return { name: item, type: "continuous", levels: "?" };
        else
            return found[0];
    });

    return newvartype;
}
