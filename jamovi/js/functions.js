const fun = {
  
  update_z_value: function( ui ) {
  
      
      if (ui.plot_z.value() === "none") {
         ui.plot_lines_values.$el.hide();
         return
      }
      
      var n_lines=ui.plot_z_lines.value();
      if (n_lines < 1) {
          ui.plot_z_lines.setValue(1);
          return
      }
      if (n_lines > 5) {
          ui.plot_z_lines.setValue(5);
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
      ui.plot_z_value.$el.css("background-color","inherit");
      ui.plot_z_value.$el.css("border","0");
      ui.plot_z_value.$el.css("height","");
      ui.plot_z_value.$el.children().width("70px");
      ui.plot_z_value.$el.css("display","contents");
      ui.plot_lines_values.$el.show();
      
  
}

  
}
module.exports=fun