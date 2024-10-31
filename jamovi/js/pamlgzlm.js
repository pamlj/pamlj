var fun=require('./functions');

const events = {
  

    update: function(ui) {
         console.log("Updating analysis");
    },

    onChange_model: function(ui) {
      
         console.log("Model changed");
         if (ui.model_type.value() == "logistic") {
           ui.nlevels.setValue(2);
         }

    }
};

module.exports = events;

