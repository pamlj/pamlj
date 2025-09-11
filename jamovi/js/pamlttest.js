var fun=require('./functions');

const events = {
  
    update: function(ui) {
         console.log("Updating analysis");
         fun.update_z_value(ui);
         this.mode_changed(ui);
    },
    aim_changed: function(ui) {
      ui.is_equi.setValue(false);
    },
    is_equi_changed: function(ui) {
      var mod = ui.mode.value();
      
      if (ui.is_equi.value() === true) { 
        if (mod == "ttestind") {
           ui.ttestind_es.setValue(0)
         }
        if (mod == "ttestpaired") {
          ui.ttestpaired_es.setValue(0)
         }
        if (mod == "ttestone") {
           ui.ttestone_es.setValue(0)
        }
      ui.plot_contour.setValue(false);
      ui.plot_escurve.setValue(false);
      ui.plot_ncurve.setValue(false);

      } else {
        
        if (mod == "ttestind") {
           ui.ttestind_es.setValue(.20)
         }
        if (mod == "ttestpaired") {
          ui.ttestpaired_es.setValue(.20)
         }
        if (mod == "ttestone") {
           ui.ttestone_es.setValue(.20)
        }
      }     
        
    },
    mode_changed: function(ui) {
      
      var self = ui.mode;
      switch (self.value()) {
        case 'ttestind':
          ui.convert_es_ttestpaired.el.style.display='none';
          ui.convert_es_ttestone.el.style.display='none';
          ui.convert_es_ttestind.el.style.display='';
          break;
        case 'ttestpaired':
          ui.convert_es_ttestpaired.el.style.display='';
          ui.convert_es_ttestone.el.style.display='none';
          ui.convert_es_ttestind.el.style.display='none';
          break;
        case 'ttestone':
          ui.convert_es_ttestpaired.el.style.display='none';
          ui.convert_es_ttestone.el.style.display='';
          ui.convert_es_ttestind.el.style.display='none';
          break;
      }

      
    },
    ttestind_nratio_changed: function(ui) {
      
        var value = ui.ttestind_nratio.value();
        if (value < 1) ui.ttestind_nratio.setValue(1);
        
    },
    ttestind_ttest_changed: function(ui) {
      var self = ui.ttestind_ttest
      var old  =  self.value();
      if (old  ===  "-")
              return;
      if (isNaN(old)) {
        self.setValue("-");
        return;
      }
      if ( +old < 0.01) {
        self.setValue("-");
        return;
      }
            
      update_convert_ind(ui);
        
     },

     ttestind_ngroup_changed: function(ui) {
       
       console.log(ui.ttestind_useit)
      var self = ui.ttestind_ngroup
      var old  =  self.value();
      if (old < 0 ) {
        self.setValue(0);
        return;
      }
      if (old === 0 ) {
        return;
      }
      update_convert_ind(ui);
     },
     ttestind_useit: function(ui) {
       console.log("useit ind");
       var d_ui = ui.ttestind_d.value();
       var d = Number(d_ui);
       if (isNaN(d)) return;
       if (d > 0 ) {
         ui.ttestind_es.setValue(d.toFixed(4));
       }
       
     },
    ttestpaired_ttest_changed: function(ui) {
      console.log("ttestpaired_ttest_changed")
      var self = ui.ttestpaired_ttest
      var old  =  self.value();
      if (old  ===  "-")
              return;
      if (isNaN(old)) {
        self.setValue("-");
        return;
      }
      if ( +old < 0.01) {
        self.setValue("-");
        return;
      }
            
      update_convert_paired(ui);
        
     },

     ttestpaired_obsn_changed: function(ui) {
       
      var self = ui.ttestpaired_obsn
      var old  =  self.value();
      if (old < 0 ) {
        self.setValue(0);
        return;
      }
      if (old === 0 ) {
        return;
      }
      update_convert_paired(ui);
     },
     ttestpaired_useit: function(ui) {
       var d_ui = ui.ttestpaired_d.value();
       var d = Number(d_ui);
       if (isNaN(d)) return;
       if (d > 0 ) {
         ui.ttestpaired_es.setValue(d.toFixed(4));
       }
       
     },
    ttestpaired_dind_changed: function(ui) {
      
   
      var self = ui.ttestpaired_dind
      var old  =  self.value();
      if (old  ===  "-")
              return;
      if (isNaN(old)) {
        self.setValue("-");
        return;
      }
      if ( +old < 0) {
        self.setValue(-old);
        return;
      }
            
      update_convert_paired_d(ui);
        
     },

     ttestpaired_r_changed: function(ui) {
       
      update_convert_paired_d(ui);
      
     },
     ttestpaired_d_useit: function(ui) {
       
       var d_ui = ui.ttestpaired_dz.value();
       var d = Number(d_ui);
       if (isNaN(d)) return;
       if (d > 0 ) {
         ui.ttestpaired_es.setValue(d.toFixed(4));
       }
       
     },
    ttestone_ttest_changed: function(ui) {
      var self = ui.ttestone_ttest
      var old  =  self.value();
      if (old  ===  "-")
              return;
      if (isNaN(old)) {
        self.setValue("-");
        return;
      }
      if ( +old < 0.01) {
        self.setValue(-old);
        return;
      }
            
      update_convert_one(ui);
        
     },

     ttestone_obsn_changed: function(ui) {
       
      var self = ui.ttestone_obsn
      var old  =  self.value();
      if (old < 0 ) {
        self.setValue(0);
        return;
      }
      if (old === 0 ) {
        return;
      }
      update_convert_one(ui);
     },
     ttestone_useit: function(ui) {
       console.log("testone use it")
       var d_ui = ui.ttestone_d.value();
       var d = Number(d_ui);
       if (isNaN(d)) return;
       if (d > 0 ) {
         ui.ttestone_es.setValue(d.toFixed(4));
       }
       
     }     


};

module.exports = events;


var update_convert_ind = function( ui) {
 
 var t =   ui.ttestind_ttest.value();
 var n =   ui.ttestind_ngroup.value();
 if (n < 4 ) return;
 var d =   +t * Math.sqrt(2/n)
 

 ui.ttestind_d.setValue(d.toFixed(4));  
}

var update_convert_paired = function( ui) {
 
 var t =   ui.ttestpaired_ttest.value();
 var n =   ui.ttestpaired_obsn.value();
 if (n < 4 ) return;
 var d =   +t / Math.sqrt(n)
 
 ui.ttestpaired_d.setValue(d.toFixed(4));  
}

var update_convert_one = function( ui) {
 
 var t =   ui.ttestone_ttest.value();
 var n =   ui.ttestone_obsn.value();
 if (n < 4 ) return;
 var d =   +t / Math.sqrt(n)
 
 ui.ttestone_d.setValue(d.toFixed(4));  
}

var update_convert_paired_d = function( ui) {
 
 var d =   ui.ttestpaired_dind.value();
 var r =   ui.ttestpaired_r.value();
 var dz =   +d /( Math.sqrt(2*(1-r)))
 
 ui.ttestpaired_dz.setValue(dz.toFixed(4));  
}


