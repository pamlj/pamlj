'use strict';

const events = {
  
    update: function(ui) {
         console.log("Updating analysis");
         update_z_value(ui);
         console.log(ui.mode.value())
         update_facpeta(ui);
    },
    mode_changed: function(ui) {
         update_facpeta(ui);
    },
    effect_type_changed: function(ui) {
         update_facpeta(ui);
         if (ui.effect_type.value()==="within") {
            ui.design_groups.setValue(1);
            ui.effect_groups.setValue(1);
         }
    },
    repeated_type_changed: function(ui) {
      if (ui.repeated_type.value()==="repeated") {
        ui.effect_groups_box.$el.hide();
        ui.design_groups.setValue(1);
        ui.effect_groups.setValue(1);

      } else {
        ui.effect_groups_box.$el.show();
        ui.design_groups.setValue(2);
        ui.effect_groups.setValue(2);
      }
      
    },
    effect_groups_changed: function(ui) {
      var val1=ui.effect_groups.value();
      if (val1<1) {
        ui.effect_groups.setValue(1);
        return
      }
      var val2=ui.design_groups.value();
      if (val2<val1) ui.design_groups.setValue(val1);
    },
     design_groups_changed: function(ui) {
    },
     df_effect_changed: function(ui) {
       
      var val1=ui.design_groups.value();
      if (val1<1) {
        ui.design_groups.setValue(1);
        return
      }
      var val2=ui.df_effect.value();
      var val3=ui.effect_groups.value();
      
      if (ui.effect_type.value()=="between") {
           if (val1<(val2+1)) ui.design_groups.setValue(val2+1);
      } else {
        if (ui.repeated_type.value()=="mixed") {
                   if (val1<val3) ui.design_groups.setValue(val3);
        } 
      }

    },

    plot_x_changed: function(ui) {
      
         ui.plot_x_from.setValue(0);
         ui.plot_x_to.setValue(0);

    },
    plot_z_changed: function(ui) {

     ui.plot_z_value.setValue([]);
     ui.plot_z_lines.setValue(0);
    },
    
    plot_z_lines_changed: function(ui) {
      
      var n_lines=ui.plot_z_lines.value();
      if (n_lines === 0) {
          ui.plot_value_label.$el.hide();
          return
      }
      
     var values = ui.plot_z_value.value();
     var n = ui.plot_z_lines.value();
     var newvalues = [];

     for (let i = 0; i < n ; i++) {

            var newval = Number(values[i]);  
            console.log(newval, typeof newval)
            if (isNaN(newval))
                  newval = 0;
            newvalues.push(newval);
     } 

      ui.plot_z_value.setValue(newvalues);
      update_z_value(ui);
      ui.plot_value_label.$el.show();
      
    },
    
    onChange_value_added: function(ui) {
      
    },
    onChange_value_removed: function(ui) {
      
    },
    
    factors_changed: function(ui) {
          updateRmSupplier(ui);
    },
    
    rmSupplier_updated: function(ui) {
          updateRmSupplier(ui);
    },
    
    rmSupplier_changed: function(ui) {

//      let values = utils.itemsToValues(ui.rmSupplier.value());
//          utils.checkValue(ui.within, true, values, FormatDef.term);

        }

};

module.exports = events;

var update_z_value = function( ui ) {
  
      ui.plot_z_value.$el.css("background-color","inherit");
      ui.plot_z_value.$el.css("border","0");
      ui.plot_z_value.$el.css("height","");
 
      if (ui.plot_z_lines.value() < 6) {
                 ui.plot_z_value.$el.css("display","contents");
      } else {
                 ui.plot_z_value.$el.css("display","block");
      }
      ui.plot_z_value.$el.children().width("70px");

  
}



var updateRmSupplier= function(ui) {
  
        var factorsList = utils.clone(ui.factors.value(), []);
        var varList=utils.valuesToItems(factorsList, FormatDef.term);
        ui.rmSupplier.setValue(varList);
  
  
}

var update_facpeta = function(ui) {

      console.log(ui.mode.value());

      if (ui.mode.value() ==="facmeans") {
        ui.panel_repeated.$el.show();
        ui.facmeans_options.$el.show();
        return
      } else {
        ui.panel_repeated.$el.hide();
        ui.facmeans_options.$el.hide();
      }
      

      console.log("update update_facpeta");
      
      if (ui.effect_type.value() === "between") {
          ui.repeated_type_box.$el.hide();
          ui.effect_groups_box.$el.show();
          if (ui.design_groups.value()<2) {
            ui.design_groups.setValue(ui.effect_groups.value());
          }
          
      } else {
          ui.repeated_type_box.$el.show();
          if (ui.repeated_type.value()==="repeated") {
                ui.effect_groups_box.$el.hide();
          } else {
                ui.effect_groups_box.$el.show();
          }
          if (ui.repeated_type.value()==="mixed") {
            
            if (ui.design_groups.value()<ui.effect_groups.value()) {
               ui.design_groups.setValue(ui.effect_groups.value());
            }
            
          }

      }
}