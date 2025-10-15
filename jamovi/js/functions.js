const fun = {
  
  update_z_value: function( ui ) {
  
      console.log("fun.update_z_value")
      if (ui.plot_z.value() === "none") {
         this.hide(ui.plot_lines_values);
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
      var el = get_el(ui.plot_z_value);
      el.style.backgroundColor="inherit";
      el.style.border="0";
      el.style.height="";
      for (const child of el.children)
              child.style.width = '70px';
      el.style.display="contents";
      fun.show(ui.plot_lines_values);
      fun.show(ui.plot_value_label);
  
 },
 
 update_r2: function( ui ) {


   if (ui.b_r2 != undefined ) {
     if (ui.b_df_model.value() > 1)
       ui.b_r2.setEnabled(true);
     else
       ui.b_r2.setEnabled(false);
   }
   
   if (ui.e_r2 != undefined ) {
     if (ui.e_df_model.value() > 1 | ui.e_df_effect.value() > 1)
       ui.e_r2.setEnabled(true);
     else
       ui.e_r2.setEnabled(false);
   }
   
 },

 make_readonly: function(obj) {
    var input = get_input(obj);
    input.readOnly=true;
    input.style.backgroundColor = "#CFECEC";
    input.style.borderColor = "#5981b3";
 },

 show: function(obj) {
    el = get_el(obj);
    el.style.display = "";
 },
 hide: function(obj) {
    el = get_el(obj);
    el.style.display = "none";
 }
 
}

module.exports=fun



var get_el=function(obj) {
    if (obj.el)                      // jamovi 2.7
        return obj.el;
    if (obj.$el && obj.$el[0])         // jamovi 2.6
        return obj.$el[0];
    return null;                   // never know
}

var get_input=function(obj) {
    if (obj.input)                      // jamovi 2.7
        return obj.input;
    if (obj.$input && obj.$input[0])         // jamovi 2.6
        return obj.$input[0];
    return null;                   // never know
}

