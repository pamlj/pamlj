var fun=require('./functions');

const events = {
  

    update: function(ui) {
         console.log("Updating analysis");
         update_structure(ui);
         update_model(ui);
         fun.update_z_value(ui);
         fun.update_r2(ui);
    },
    mode_changed: function(ui) {
        console.log("mode has changed in " + ui.mode.value());
        update_structure(ui);
        fun.update_r2(ui);

    },
    b_df_model_changed: function(ui){
      console.log("b_df_model update");
      fun.update_r2(ui);
    },
    onChange_factors: function(ui) {
      
         update_model(ui);

    },
    
    onChange_factors_list_change: function(ui) {
      console.log("list changed");
      update_df(ui);
      
    },
    onChange_mode: function(ui) {
      console.log("mode changed");
       update_structure(ui);

    },
    onChange_convert: function(ui) {
      console.log("convert changed");
       update_convert(ui);

    },
    onChange_rx: function(ui) {
      console.log("rx changed");
      var rx = ui.rx.value();
      if (rx.length > 0 ) {
            ui.covs.setValue(rx.length)
      }
       update_convert(ui);

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

var update_convert = function( ui) {

   if (ui.mode.value() !== "peta" ) {
     return
   } 
  
   console.log("converting ES");
   var eta = ui.eta.value();
   if (eta === 0) {
     ui.omega.setValue(0);
     ui.epsilon.setValue(0);
     ui.gpower.setValue(0);
     return
   }
   var df = ui.v_df_effect.value();
   if (df === 0) return
   var df_model = ui.v_df_model.value();
   var df_error = ui.eta_df_error.value();
   
   if (df_error === 0) return
   
   var f = eta*df_error/((1-eta)*df)
   var omega = ((f - 1) * df)/( (f -1 ) * df + df_model + df_error + 1);
   var epsilon = ((f - 1) * df)/(f * df + df_error) ;
   var k = df+1
   var N = df_model + df_model + df_error + 1
   var gpower = eta*(k-N)/((eta*k)-N)

console.log(omega)
   ui.omega.setValue(omega.toFixed(3));
   ui.epsilon.setValue(epsilon.toFixed(3));
   ui.gpower.setValue(gpower.toFixed(3));

   var obj = ui.v_es;

   if (ui.use.value() === "epsilon")
       obj.setValue(epsilon.toFixed(3));
   if (ui.use.value() === "omega")
       obj.setValue(omega.toFixed(3));
   if (ui.use.value() === "gpower")
       obj.setValue(gpower.toFixed(3));
       
   if (ui.use.value() === "f2" && ui.f2.value() > 0) {
      var f2 = ui.f2.value();
          eta = f2/(1+f2);
          obj.setValue(eta.toFixed(3));
   }


}

var update_structure = function( ui) {
       
        if (["beta","eta"].includes(ui.mode.value())) {
          ui.convert_es.$el.hide() ;
          ui.use.setValue("none") ;
          if (ui.b_df_model.value() < 1) 
              ui.b_df_model.setValue(1);
        }
        if (ui.mode.value() === "peta") {
          ui.convert_es.$el.show();
          ui.use.setValue("none") ;        
          ui.omega.$input.prop("readonly",true);
          ui.omega.$input.css("background-color","#CFECEC");
          ui.omega.$input.css("border-color","#5981b3");
        
          ui.epsilon.$input.prop("readonly",true);
          ui.epsilon.$input.css("background-color","#CFECEC");
          ui.epsilon.$input.css("border-color","#5981b3");

          ui.gpower.$input.prop("readonly",true);
          ui.gpower.$input.css("background-color","#CFECEC");
          ui.gpower.$input.css("border-color","#5981b3");

        }
        
        if (ui.mode.value() === "beta") {
          ui.panel_correlations.$el.show()
          
        } else {
          ui.panel_correlations.$el.hide()
          
        }

        
        if (typeof ui.plot_value_label !== "undefined" ) {
          
          var z = ui.plot_z.value();
          if (z === "none") {
            ui.plot_value_label.$el.hide();
          }
          var zv = ui.plot_z_value.value();
          if (z === 0) {
            ui.plot_value_label.$el.hide();
          }
          
        }
        

  
}
    

var update_model = function( ui) {

      console.log("factors changed");
      var nfactors = ui.factors.value();

      if ( nfactors == 0) {
        ui.factors_group.$el.hide();
        ui.factors_list.setValue([]);
        return
        
      }
      
      ui.factors_group.$el.show();
      var factors = ui.factors_list.value();   
      console.log(factors)

      var newarray = new Array(nfactors).fill(0)
      newarray.forEach(function(value, i, arr){
        if (typeof factors[i] === 'undefined')
               arr[i] = {var: "factor " + (i+1), levels: 0};
        else 
               arr[i] = factors[i]
      });
      ui.factors_list.setValue(newarray);


}

var update_df = function( ui) {

      if ( ui.covs.value()+ui.factors.value() === 0) return
      console.log("updating the df");
      var factors = ui.factors_list.value();   
      var df_factors=[];
      factors.forEach((value) => {
        if (value.levels > 1) df_factors.push(value.levels-1);
      });
      var df1 = 0;
      if (df_factors.length > 0 ) {
          var inter = getCombinations(df_factors);
          var order = order_num(ui.factors_order.value());
              inter = inter.filter(obj => {return obj.length < order});
              df1= inter.map( (value) => value.reduce( (a,b) => a*b)).reduce( (a,b) => a+b);
      }
      var ncovs=ui.covs.value();
      var df_covs= []
      var df2 = 0;
      if (ncovs > 0 ) {
            df_covs = new Array(ncovs).fill(1);
        var order=order_num(ui.covs_order.value());
        var inter = getCombinations(df_covs);
            inter = inter.filter(obj => {return obj.length < order});
            df2= inter.map( (value) => value.reduce( (a,b) => a*b)).reduce( (a,b) => a+b);
      }    

      var df3 = 0;
          order = order_num(ui.mixed_order.value());
      if (order > 1 && df_factors.length > 0 &&  df_covs.length > 0) {
           var covs_comb = getCombinations(df_covs);
           var fact_comb = getCombinations(df_factors);
               inter = covs_comb.flatMap(d => fact_comb.map(v => d.concat(v)));
               var sel = inter.filter((obj) =>  obj.length < order);
               df3=  sel.map( (value) => value.reduce( (a,b) => a*b)).reduce( (a,b) => a+b);
      }
    console.log(df3)

     var df = df1+df2+df3 ;
     if ( df > 0 ) {
       ui.b_df_model.setValue(df);
       ui.v_df_model.setValue(df);
       ui.e_df_model.setValue(df);

     }

     console.log("df_model updated to: " + df);
}

var order_num = function(avalue) {
  
  if (avalue==="main")
     return(2)
  if (avalue==="order2")
     return(3)
  if (avalue==="order3")
     return(4)
  if (avalue==="orderall")
     return(1000) // if you want order>1000 you need to think it twice
  if (avalue==="none")
     return(1)

}
var getCombinations = function(valuesArray) {

    var combi = [];
    var temp = [];
    var slent = Math.pow(2, valuesArray.length);
  
    for (var i = 0; i < slent; i++)  {
        temp = [];
        for (var j = 0; j < valuesArray.length; j++) {
            if ((i & Math.pow(2, j))) {
                temp.push(valuesArray[j]);
            }
        }
        if (temp.length > 0)  {
            combi.push(temp);
        }
    }
    
    combi.sort((a, b) => a.length - b.length);
return combi;
}