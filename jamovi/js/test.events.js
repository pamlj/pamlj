const moveup=0;
const movedown=0;
const bound=2000000;
const events = {
    update: function(ui) {
        console.log("overall update");
        ui.factors.$el.css("border","0");
        ui.factors.$el.css("background-color","inherit");
        ui.covs.$el.css("border","0");
        ui.covs.$el.css("background-color","inherit");
        ui.clusters.$el.css("border","0");
        ui.clusters.$el.css("background-color","inherit");
        ui.note.$el.children().css( "font-weight", "normal");
        updateStructure(ui,this);

    },
    onChange_name: function(ui) {
      console.log("name changed")
      updateStructure(ui,this);
    
    },
                  
    onChange_covs_added: function(ui) {
      
      var h = ui.covs.$el.height();
      ui.covs.$el.height(h+moveup)
    },
    onChange_covs_removed: function(ui) {
      
      var h = ui.covs.$el.height();
      if (h>bound)
          ui.covs.$el.height(h-movedown)
      updateStructure(ui,this);
      
    },
    onChange_factors_added: function(ui) {
      
      var h = ui.factors.$el.height();
      ui.factors.$el.height(h+moveup)
      updateStructure(ui,this);

    },
    onChange_factors_removed: function(ui) {
      
      var h = ui.factors.$el.height();
      if (h>bound)
          ui.factors.$el.height(h-movedown)
      updateStructure(ui,this);
      
    },
    
    onChange_clusters_added: function(ui) {
      
      var h = ui.clusters.$el.height();
      ui.clusters.$el.height(h+moveup)
      updateStructure(ui,this);
      
    },
    onChange_clusters_removed: function(ui) {
      
      var h = ui.clusters.$el.height();
      if (h>bound)
          ui.clusters.$el.height(h-movedown)
      updateStructure(ui,this);
      
    },


    onChange_structure: function(ui) {
       console.log("structure changed");
       updatePairs(ui,this);

    },
    onChange_structureSupplier: function(ui) {
        console.log("structureSupplier changed");
      
    },
    onUpdate_structureSupplier: function(ui) {
       console.log("structureSupplier updated");
       updatePairs(ui,this);

    },

    onChange_items_changed: function(ui) {
       console.log("items changed");

    },
    onChange_doNothing: function(ui) {
        console.log("do nothing");

    }
};

const updateStructure=function(ui, obj) {
  
  var context=obj.base;
  console.log("updateStructure function");
  var factors=context.cloneArray(ui.factors.value(),[]);
  var covs=context.cloneArray(ui.covs.value(),[]);
  for (var i = 0; i< covs.length; i++) {covs[i]={var:covs[i],opt1:null}};
  var clusters=context.cloneArray(ui.clusters.value(),[]);

  var vars=factors.concat(covs).concat(clusters);

  var names=[];
  var custom=[];
  
  for (var i = 0; i < vars.length; i++) {

    if (vars[i].var!==null) {
        custom.push({name: vars[i].var, measureType: 'none', dataType: 'none', levels: [] })
        names.push(vars[i].var)
    }

  }
  context.setCustomVariables(custom);
  ui.structureSupplier.setValue(context.valuesToItems(names, FormatDef.variable));


};

const updatePairs=function(ui, obj) {
  
  var context=obj.base;
  console.log("updatePairs function");
  var factors=context.cloneArray(ui.factors.value(),[]);
  var covs=context.cloneArray(ui.covs.value(),[]);
  for (var i = 0; i< covs.length; i++) {covs[i]={var:covs[i],opt1:null}};
  var clusters=context.cloneArray(ui.clusters.value(),[]);
  var vars=factors.concat(covs);
  var pairs=context.cloneArray(ui.structure.value(),[]);  
  var newpairs=[]
  var changed=false;
  for (var i = 0; i < pairs.length; i++) {
    var test1=context.listContains(vars,pairs[i].variable,undefined,"var");
    var test2=(pairs[i].cluster===null || context.listContains(clusters,pairs[i].cluster,undefined,"var"));
    if (!test2) {  
         pairs[i].cluster=null;
         changed=true;
    }
    if (test1) {
            newpairs.push(pairs[i]);
    } else
        changed=true;
  }
  console.log(newpairs);
  console.log(changed);
  if (changed)
     ui.structure.setValue(newpairs);
  var clusters=context.cloneArray(ui.clusters.value(),[]);
};




module.exports = events;
