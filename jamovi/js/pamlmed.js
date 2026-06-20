var fun=require('./functions');

const events = {
  
    view_updated: function(ui) {
         console.log("Updating analysis");
         fun.update_z_value(ui);
         update_coefs(ui);
         update_sensitivity_coef(ui);
         update_mde_section(ui);

    },
    aim_changed: function(ui) {
      
      var value = ui.aim.value();
      if (value === "es") {
        ui.table_pwbyn.setValue(false);
        ui.table_pwbyes.setValue(true);
      }
      if (value === "n") {
        ui.table_pwbyn.setValue(true);
        ui.table_pwbyes.setValue(false);
      }
      
      
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
    mode_changed: function(ui) {

      if (ui.mode.value() === "medcomplex") {
        ui.diagram.setValue(true);
      }
      update_sensitivity_coef(ui);
      update_mde_section(ui);
    },
    model_type_changed: function(ui) {
    console.log("model_type_changed");
    update_coefs(ui);
    update_sensitivity_coef(ui);


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

// The Sensitivity "Coefficient to vary" list depends on the model: the simple
// model has a / b; the complex models have the coefficients of the chosen
// model_type. Repopulate the ComboBox accordingly and, when the current value
// is no longer valid, fall back to the first coefficient. Free (medmodels)
// models choose the coefficient through the `test:` directive in the syntax,
// so the list is left untouched there.
var update_sensitivity_coef = function(ui) {

      var mode = ui.mode.value();
      var opts;

      if (mode === "medsimple") {
        opts = ["a", "b"];
      } else if (mode === "medcomplex") {
        var mt = ui.model_type.value();
        if (mt === "threemeds")      opts = ["a1", "a2", "a3", "b1", "b2", "b3"];
        else if (mt === "twoserial") opts = ["a1", "a2", "b1", "b2", "d1"];
        else                         opts = ["a1", "a2", "b1", "b2"];   // twomeds
      } else {
        return;   // medmodels: coefficient chosen via the test: directive
      }

      var optionList = opts.map(function(o) { return { name: o, title: o }; });
      ui.sensitivity_coef.setPropertyValue("options", optionList);

      if (opts.indexOf(ui.sensitivity_coef.value()) === -1)
        ui.sensitivity_coef.setValue(opts[0]);
}

// Free (medmodels) models choose the coefficient to vary through the `test:`
// directive in the model syntax, so the "Minimum detectable effect" section
// (the Coefficient-to-vary list) is hidden for that mode.
var update_mde_section = function(ui) {
      if (ui.mode.value() === "medmodels")
        ui.mde_section.$el.hide();
      else
        ui.mde_section.$el.show();
}
