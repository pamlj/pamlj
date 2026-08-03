// thias should work in 2.6 and 2.7
var fun = require('./functions');

const events = {

    update: function(ui) {
        console.log('Updating analysis');
        ensure_mode_watcher(ui);
        update_structure(ui);
        update_model(ui);
        update_df(ui);
        fun.update_z_value(ui);
    },
    y_levels_changed: function(ui) {
      
        const k = ui.y_levels.value();
        ui.y_prop.setValue(Number(1/k));

    },
    model_type_changed: function(ui) {
      
      const mt = ui.model_type.value();
      if (mt=="logistic") {
         ui.y_levels.setValue(2);
      }
      else {
        const k = ui.y_levels.value();
        if (k<3) ui.y_levels.setValue(3);
        ui.y_prop.setValue(Number(1/Number(ui.y_levels.value())));
      }

    },

    onChange_factors: function(ui) {
        update_model(ui);
        update_df(ui);
    },

    onChange_factors_list_change: function(ui) {
        console.log('list changed');
        update_df(ui);
    },

    onChange_mode: function(ui) {
        update_structure(ui);
    }

};

module.exports = events;

var ensure_mode_watcher = function(ui) {

    if (ui.__pamlgzlmModeWatcher)
        return;

    ui.__pamlgzlmModeWatcher = true;

    var last = null;
    var sync = function() {
        if (!ui.mode || !ui.mode.value)
            return;

        var current = ui.mode.value();
        if (current !== last) {
            last = current;
            update_structure(ui);
        }
    };

    sync();

    if (ui.mode.on)
        ui.mode.on('change', sync);

    var el = fun.get_el(ui.mode);
    if (el && el.addEventListener) {
        el.addEventListener('change', sync);
        el.addEventListener('click', sync);
    }

    if (typeof setInterval !== 'undefined')
        ui.__pamlgzlmModeTimer = setInterval(sync, 250);
};

var update_structure = function(ui) {

    if (ui.mode.value() === 'r2')
        fun.show(ui.panel_model);
    else
        fun.hide(ui.panel_model);
};

var update_model = function(ui) {

    console.log('factors changed');
    var nfactors = ui.factors.value();

    if (nfactors == 0) {
        fun.hide(ui.factors_group);
        ui.factors_list.setValue([]);
        update_df(ui);
        return;
    }

    ui.factors_group.$el.show();
    var factors = ui.factors_list.value();
    console.log(factors);

    var newarray = new Array(nfactors).fill(0);
    newarray.forEach(function(value, i, arr) {
        if (typeof factors[i] === 'undefined')
            arr[i] = { var: 'factor ' + (i + 1), levels: 0 };
        else
            arr[i] = factors[i];
    });
    ui.factors_list.setValue(newarray);
};

var update_df = function(ui) {

    if (ui.covs.value() + ui.factors.value() === 0) {
        ui.r2_df.setValue(1);
        return;
    }

    console.log('updating the df');
    var factors = ui.factors_list.value();
    var df_factors = [];
    factors.forEach((value) => {
        if (value.levels > 1) df_factors.push(value.levels - 1);
    });

    var df1 = 0;
    if (df_factors.length > 0) {
        var inter = getCombinations(df_factors);
        var order = order_num(ui.factors_order.value());
        inter = inter.filter(obj => { return obj.length < order; });
        df1 = inter.map((value) => value.reduce((a, b) => a * b)).reduce((a, b) => a + b);
    }

    var ncovs = ui.covs.value();
    var df_covs = [];
    var df2 = 0;
    if (ncovs > 0) {
        df_covs = new Array(ncovs).fill(1);
        var order = order_num(ui.covs_order.value());
        var inter = getCombinations(df_covs);
        inter = inter.filter(obj => { return obj.length < order; });
        df2 = inter.map((value) => value.reduce((a, b) => a * b)).reduce((a, b) => a + b);
    }

    var df3 = 0;
    order = order_num(ui.mixed_order.value());
    if (order > 1 && df_factors.length > 0 && df_covs.length > 0) {
        var covs_comb = getCombinations(df_covs);
        var fact_comb = getCombinations(df_factors);
        inter = covs_comb.flatMap(d => fact_comb.map(v => d.concat(v)));
        var sel = inter.filter((obj) => obj.length < order);
        df3 = sel.map((value) => value.reduce((a, b) => a * b)).reduce((a, b) => a + b);
    }

    var df = df1 + df2 + df3;
    if (df > 0) {
        ui.r2_df.setValue(df);
    }

    console.log('df updated to: ' + df);
};

var order_num = function(avalue) {

    if (avalue === 'main')
        return 2;
    if (avalue === 'order2')
        return 3;
    if (avalue === 'order3')
        return 4;
    if (avalue === 'orderall')
        return 1000;
    if (avalue === 'none')
        return 1;
};

var getCombinations = function(valuesArray) {

    var combi = [];
    var temp = [];
    var slent = Math.pow(2, valuesArray.length);

    for (var i = 0; i < slent; i++) {
        temp = [];
        for (var j = 0; j < valuesArray.length; j++) {
            if ((i & Math.pow(2, j))) {
                temp.push(valuesArray[j]);
            }
        }
        if (temp.length > 0) {
            combi.push(temp);
        }
    }

    combi.sort((a, b) => a.length - b.length);
    return combi;
};
