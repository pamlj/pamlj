var fun=require('./functions');

const events = {
  
    view_updated: function(ui) {
         console.log("Updating analysis");
         fun.update_z_value(ui);
    },
    test_changed: function(ui) {
      console.log("test changed")
      var value = ui.test.value();
      if (value === "mc") {
        ui.table_pwbyn.setValue(false);
        ui.plot_ncurve.setValue(false);
      } else {
        ui.table_pwbyn.setValue(true);
      }
      
    }


};

module.exports = events;

