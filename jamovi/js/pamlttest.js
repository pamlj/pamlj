
const events = {
  
    update: function(ui) {
         console.log("Updating analysis");
         update_z_value(ui);
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





