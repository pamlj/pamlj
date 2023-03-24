var rtermFormat = require('./rtermFormat');
var fun=require('./functions');

const events = {
    update: function(ui) {
        this.setCustomVariable("Intercept", "none", "");
        fun.calcModelTerms(ui, this);
        fun.filterModelTerms(ui, this);
        fun.updateFixedSizes(ui,this);
    },

    onChange_factors: function(ui) {
        fun.calcModelTerms(ui, this);
        fun.updateRandomSupplier(ui,this);
    },

    onChange_covariates: function(ui) {
        fun.calcModelTerms(ui, this);
        fun.updateRandomSupplier(ui,this);
    },

    onChange_model_terms: function(ui) {
        fun.filterModelTerms(ui, this);
        fun.updateRandomSupplier(ui,this);
        fun.updateFixedSizes(ui,this);

    },

    onChange_model: function(ui) {

        if (typeof ui.es_RR !== 'undefined' ) {
              ui.es_RR.setValue(false);
        }
        if (typeof ui.plot_scale !== 'undefined' ) {
              ui.plot_scale.setValue('response');
        }

        if (ui.model_type.getValue()==="custom" ||  ui.model_type.getValue()==="linear")        {
               ui.es_expb.setValue(false);
               ui.estimates_ci.setValue(true);
               ui.expb_ci.setValue(false);
        } else  {
               ui.es_expb.setValue(true);
               ui.estimates_ci.setValue(false);
               ui.expb_ci.setValue(true);
        }

        if (typeof ui.propodds_test !== 'undefined') {
              if (ui.model_type.getValue()==="ordinal") {
                  ui.propodds_test.$el.show();
              } else {
                  ui.propodds_test.$el.hide();
              }
        }


  
        if (typeof ui.es_marginals !== 'undefined') {
          
              ui.es_marginals.setValue(false);

        }
        
        ui.dep.setValue(null);
      },

    onChange_model_remove: function(ui) {
      let values=this.cloneArray(ui.model_terms.value(),[]);

    },

    onUpdate_modelSupplier: function(ui) {
            let factorsList = this.cloneArray(ui.factors.value(), []);
            let covariatesList = this.cloneArray(ui.covs.value(), []);
            var variablesList = factorsList.concat(covariatesList);
            ui.modelSupplier.setValue(this.valuesToItems(variablesList, FormatDef.variable));

    },
    
    onChange_cluster: function(ui) {
        fun.updateRandomSupplier(ui,this);
    },

    onChange_randomSupplier: function(ui){
      
        let supplierList = this.itemsToValues(ui.randomSupplier.value());
        var changes = this.findChanges("randomSupplier",supplierList,rtermFormat);
        if (changes.removed.length>0) {
          var re = this.cloneArray(ui.re.value(),[]);
          var  light = removeFromMultiList(changes.removed,re,this,1);
          ui.re.setValue(light);
        }
        return;
    },
    
    onUpdate_randomSupplier: function(ui) {
        fun.updateRandomSupplier(ui,this);

    },
    
    onEvent_re_list: function(ui) {
      fun.updateRandomSupplier(ui,this);
    },
    
    onEvent_corr: function(ui, data) {
          console.log("Correlation structure changed");
          fun.fixRandomEffects(ui,this);
    },    

    onEvent_addRandomTerm: function(ui) {
//        console.log("addRandomTerm does nothing");
    },

    onChange_es: function(ui, data) {
      
//      console.log(ui.fixed_sizes)
      fun.updateFixedSizes(ui,this);

    },    

    onEvent_nothing: function(ui, data) {
//          console.log("I didn't do anything");
    }    

};


module.exports = events;



// local functions 


var removeFromList = function(quantum, cosmos, context, order = 1) {

     cosmos=normalize(cosmos);
     quantum=normalize(quantum);
     if (cosmos===undefined)
        return([]);
     var cosmos = context.cloneArray(cosmos);
       for (var i = 0; i < cosmos.length; i++) {
          if (cosmos[i]===undefined)
             break;
          var aCosmos = context.cloneArray(cosmos[i]);
           for (var k = 0; k < quantum.length; k++) {
             var  test = order === 0 ? FormatDef.term.isEqual(aCosmos,quantum[k]) : FormatDef.term.contains(aCosmos,quantum[k]);
                 if (test && (aCosmos.length >= order)) {
                        cosmos.splice(i, 1);
                        i -= 1;
                    break;    
                    }
          }
            
       }
  
    return(cosmos);
};



var removeFromMultiList = function(quantum, cosmos, context, strict = 1) {

    var cosmos = context.cloneArray(cosmos);
    var dimq = dim(quantum);
        for (var j = 0; j < cosmos.length; j++) 
           cosmos[j]=removeFromList(quantum,cosmos[j],context, strict);
    return(cosmos);
};

var dim = function(aList) {

    if (!Array.isArray(aList))
           return(0);
    if (!Array.isArray(aList[0]))
           return(1);
    if (!Array.isArray(aList[0][0]))
           return(2);
    if (!Array.isArray(aList[0][0][0]))
           return(3);
    if (!Array.isArray(aList[0][0][0][0]))
           return(4);

  
    return(value);
};

var normalize = function(cosmos) {

  if (cosmos===undefined)
          return [];
  if (dim(cosmos)===0)
          cosmos=[cosmos]
          
        for (var i = 0; i < cosmos.length; i++) {
            var aValue = cosmos[i];
            var newValue=dim(aValue)>0 ? aValue : [aValue];
            cosmos[i]=newValue
        }
        return cosmos;
}

