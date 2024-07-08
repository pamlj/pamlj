var fun=require('./functions');

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
    
    plot_x_changed: function(ui) {
      
        var plotx = ui.plot_x.value();
         
         switch (plotx) {
              case "n": 
                   ui.plot_x_from.setValue(10);
                   ui.plot_x_to.setValue(100);
                   break;
              case "power": 
                   ui.plot_x_from.setValue(.50);
                   ui.plot_x_to.setValue(.98);
                   break;
              case "es": 
                   ui.plot_x_from.setValue(0.05);
                   ui.plot_x_to.setValue(0.90);
                   break;
              case "alpha": 
                   ui.plot_x_from.setValue(0.001);
                   ui.plot_x_to.setValue(0.10);
                   break;

             default: 
                   ui.plot_x_from.setValue(0);
                   ui.plot_x_to.setValue(0);

           }
         

    },
    plot_z_changed: function(ui) {

     ui.plot_z_value.setValue([]);
     ui.plot_z_lines.setValue(1);
     fun.update_z_value(ui);
    },
    
    plot_z_lines_changed: function(ui) {

     console.log("plot_z_lines changed")      
     

      var n_lines=ui.plot_z_lines.value();
      if (n_lines < 1) {
          ui.plot_z_lines.setValue(1);
          return
      }
      if (n_lines > 5) {
          ui.plot_z_lines.setValue(5);
          return
      }
      fun.update_z_value(ui);

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





