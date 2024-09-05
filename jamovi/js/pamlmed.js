var fun=require('./functions');

const events = {
  
    view_updated: function(ui) {
         console.log("Updating analysis");
         fun.update_z_value(ui);
         update_coefs(ui);        

    },
    test_changed: function(ui) {

      var value = ui.test.value();
      if (value === "mc") {
        ui.table_pwbyn.setValue(false);
        ui.plot_ncurve.setValue(false);
      } else {
        ui.table_pwbyn.setValue(true);
      }
      
    },
    model_type_changed: function(ui) {
    console.log("model_type_changed");
    update_coefs(ui);      
      
      
    }    


};

module.exports = events;

var update_coefs= function(ui) {
  
      console.log("update_coefs");
      var value = ui.model_type.value();
      console.log(value);
      
      if (value == "twomeds") {
        ui.mediators_beta.$el.hide();
        ui.mediators_corr.$el.show();
        ui.mediator3_corr.$el.hide();
        ui.mediator3_beta.$el.hide();
        
      }
      if (value == "threemeds") {
        ui.mediators_beta.$el.hide();
        ui.mediators_corr.$el.show();
        ui.mediator3_corr.$el.show();
        ui.mediator3_beta.$el.show();
      }
      if (value == "twoserial") {
        ui.mediators_beta.$el.show();
        ui.mediator3_beta.$el.hide();
        ui.mediators_corr.$el.hide();
        ui.mediator2_sbeta.$el.hide();

      }
      
  
  
  
}
