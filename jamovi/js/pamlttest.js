var fun=require('./functions');

const events = {
  
    update: function(ui) {
         console.log("Updating analysis");
         fun.update_z_value(ui);
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
    ttestind_nratio_changed: function(ui) {
      
        var value = ui.ttestind_nratio.value();
        if (value < 1) ui.ttestind_nratio.setValue(1);
        
    }



};

module.exports = events;







