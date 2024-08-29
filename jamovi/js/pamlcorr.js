var fun=require('./functions');

const events = {
  
    view_updated: function(ui) {
         console.log("Updating analysis");
         fun.update_z_value(ui);
    }


};

module.exports = events;

