#' Mediation
#'
#' Power analysis for simple and multiple mediation models using the Sobel test,
#' joint-significance, or bootstrap confidence intervals.
#'
#' @param aim The aim of the analysis: \code{n} (default) for sample size,
#'   \code{power} to estimate power, \code{es} for effect size (correlation)
#' @param mode Mediation model mode: \code{"medsimple"} for simple mediation
#'   with paths \code{a}, \code{b}, and \code{cprime}, \code{"medcomplex"}
#'   for multiple/serial mediator models, or \code{"medmodels"} for a free
#'   model specified with the model syntax (see \code{code}).
#' @param code Character string with the model syntax used when
#'   \code{mode="medmodels"}: one regression equation per endogenous variable
#'   (e.g. \code{"m ~ .3*x\\ny ~ .2*x + .4*m"}), optionally with \code{cor()}
#'   directives and symbolic coefficient labels.
#' @param a The expected standardized effect of the independent variable on
#'   the mediator
#' @param b The expected standardized effect of the independent variable on
#'   the mediator
#' @param cprime The expected standardized effect of the independent variable
#'   on the mediator
#' @param model_type Type of complex mediation model when
#'   \code{mode="medcomplex"}: \code{"twomeds"} for two parallel mediators,
#'   \code{"threemeds"} for three parallel mediators, or
#'   \code{"twoserial"} for a two-mediator serial model.
#' @param a1 The expected standardized effect of the independent variable on
#'   mediator 1
#' @param b1 The expected standardized effect of the independent variable on
#'   mediator 1
#' @param a2 The expected standardized effect of the independent variable on
#'   mediator 1
#' @param b2 The expected standardized effect of the independent variable on
#'   mediator 1
#' @param a3 The expected standardized effect of the independent variable on
#'   mediator 1
#' @param b3 The expected standardized effect of the independent variable on
#'   mediator 1
#' @param d1 The expected standardized effect of the independent variable on
#'   mediator 1
#' @param d2 The expected standardized effect of the independent variable on
#'   mediator 1
#' @param r12 The expected standardized effect of the independent variable on
#'   mediator 1
#' @param r13 The expected standardized effect of the independent variable on
#'   mediator 1
#' @param r23 The expected standardized effect of the independent variable on
#'   mediator 1
#' @param cprime2 The expected standardized effect of the independent variable
#'   on the mediator
#' @param sensitivity_coef For complex mediation models with the effect-size
#'   aim, the path coefficient (\code{"a1"}, \code{"b1"}, \code{"d1"}, ...) that
#'   is resized to find the minimum detectable indirect effect.
#' @param power Minimal desired power
#' @param n Sample size
#' @param sig.level Type I error rate (significance cut-off or alpha)
#' @param alternative Test direction: \code{"two.sided"} (default) or
#'   \code{"one.sided"}.
#' @param test Mediation test method: \code{"sobel"}, \code{"joint"} for
#'   joint-significance, or the Monte Carlo confidence-interval methods
#'   \code{"parametric"} and \code{"simulation"} (both approximate bootstrap-CI
#'   power for the indirect effect).
#' @param mcR Number of simulated studies used by the Monte Carlo CI methods.
#' @param parallel Logical; if \code{TRUE}, use parallel computation for
#'   simulation-based methods when available.
#' @param set_seed Logical; if \code{TRUE}, use the value in \code{seed} to
#'   make simulations reproducible.
#' @param seed Random seed used when \code{set_seed=TRUE}.
#' @param table_pwbyn Logical; if \code{TRUE}, produce the "Power by Sample
#'   size" table.
#' @param table_pwbyes Logical; if \code{TRUE}, produce the "Power by Effect
#'   Size" table: the indirect effect and the value of the varied coefficient
#'   reaching power .5/.8/.95 at the analysis sample size.
#' @param plot_ncurve Logical; if \code{TRUE}, produce the power-by-\code{n}
#'   curve.
#' @param plot_log Logical; if \code{TRUE}, use a log scale where supported in
#'   plots.
#' @param plot_palette Color palette used for plots.
#' @param plot_x Parameter shown on the X axis of the custom power plot.
#' @param plot_y Parameter shown on the Y axis of the custom power plot.
#' @param plot_custom_labels Logical; if \code{TRUE}, add value labels to the
#'   custom plot.
#' @param plot_z Parameter used to split the custom power plot into multiple
#'   lines or conditions.
#' @param plot_x_from Lower bound of the X-axis parameter range for the custom
#'   power plot.
#' @param plot_x_to Upper bound of the X-axis parameter range for the custom
#'   power plot.
#' @param plot_z_lines Number of reference lines or levels to use for
#'   \code{plot_z} in the custom plot.
#' @param plot_z_value Explicit values of the \code{plot_z} parameter to use in
#'   the custom plot.
#' @param plot_to_table Logical; if \code{TRUE}, output the plotted custom
#'   power values as a table.
#' @param test_c Logical; if \code{TRUE}, also compute the power analysis for
#'   the direct X -> Y effect (\code{c} path) without mediators.
#' @param explain Logical; if \code{TRUE}, include explanatory output where
#'   available.
#' @param inspect_cors Logical; if \code{TRUE} (free models), add a table of the
#'   model-implied correlations among the variables.
#' @param diagram Logical; if \code{TRUE}, display the mediation path diagram.
#' @param .interface Used for internal purposes
#' @param .caller Used for internal purposes
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$intro} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$extrainfo} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$issues} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$initnotes} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$diagram} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$powertab} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$effectsize} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$powerbyn} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$powerxy} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$plotnotes} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$powerNcurve} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$powerCustom} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$customnotes} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$customtable} \tab \tab \tab \tab \tab a table \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$powertab$asDF}
#'
#' \code{as.data.frame(results$powertab)}
#'
#' @export
pamlmed <- function(
    aim = "n",
    mode = "medsimple",
    code = "",
    a = 0.3,
    b = 0.3,
    cprime = 0,
    model_type = "twomeds",
    a1 = "",
    b1 = "",
    a2 = "",
    b2 = "",
    a3 = "",
    b3 = "",
    d1 = "",
    d2 = "",
    r12 = "",
    r13 = "",
    r23 = "",
    cprime2 = 0,
    sensitivity_coef = "a",
    power = 0.9,
    n = 100,
    sig.level = 0.05,
    alternative = "two.sided",
    test = "joint",
    mcR = 1000,
    parallel = FALSE,
    set_seed = FALSE,
    seed = 42,
    table_pwbyn = TRUE,
    table_pwbyes = FALSE,
    plot_ncurve = FALSE,
    plot_log = FALSE,
    plot_palette = "viridis",
    plot_x = "none",
    plot_y = "none",
    plot_custom_labels = FALSE,
    plot_z = "none",
    plot_x_from = 0,
    plot_x_to = 0,
    plot_z_lines = 1,
    plot_z_value = list(),
    plot_to_table = FALSE,
    test_c = FALSE,
    explain = FALSE,
    inspect_cors = FALSE,
    diagram = TRUE) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("pamlmed requires jmvcore to be installed (restart may be required)")
    if(missing(table_pwbyn) & aim=="es"){
      table_pwbyn=FALSE
      table_pwbyes=TRUE
    }

    ## `sensitivity_coef` must be one of the coefficients of the chosen model
    ## (the option is shared across all modes). In jamovi the editor restricts the
    ## list per model; for the R interface we validate here. An incoherent value
    ## that the user supplied explicitly is ignored (with a warning) and replaced
    ## by the model's default; a value left at the default is set silently. Free
    ## (medmodels) models pick the coefficient through the `test:` directive in the
    ## syntax, so `sensitivity_coef` is not validated there.
    valid_coef <- if (mode == "medsimple") c("a", "b")
                  else if (mode == "medcomplex") switch(model_type,
                        twomeds   = c("a1", "a2", "b1", "b2"),
                        threemeds = c("a1", "a2", "a3", "b1", "b2", "b3"),
                        twoserial = c("a1", "a2", "b1", "b2", "d1"),
                        character(0))
                  else NULL
    if (!is.null(valid_coef) && length(valid_coef) > 0) {
        if (missing(sensitivity_coef)) {
            sensitivity_coef <- valid_coef[1]
        } else if ( ! sensitivity_coef %in% valid_coef) {
            warning("sensitivity_coef '", sensitivity_coef, "' is not a coefficient of mode '",
                    mode, if (mode == "medcomplex") paste0("' / model_type '", model_type),
                    "'. Ignoring it and using '", valid_coef[1], "' instead. Valid coefficients: ",
                    paste(valid_coef, collapse = ", "), ".")
            sensitivity_coef <- valid_coef[1]
        }
    }


    options <- pamlmedOptions$new(
        aim = aim,
        mode = mode,
        code = code,
        a = a,
        b = b,
        cprime = cprime,
        model_type = model_type,
        a1 = a1,
        b1 = b1,
        a2 = a2,
        b2 = b2,
        a3 = a3,
        b3 = b3,
        d1 = d1,
        d2 = d2,
        r12 = r12,
        r13 = r13,
        r23 = r23,
        cprime2 = cprime2,
        sensitivity_coef = sensitivity_coef,
        power = power,
        n = n,
        sig.level = sig.level,
        alternative = alternative,
        test = test,
        mcR = mcR,
        parallel = parallel,
        set_seed = set_seed,
        seed = seed,
        table_pwbyn = table_pwbyn,
        table_pwbyes = table_pwbyes,
        plot_ncurve = plot_ncurve,
        plot_log = plot_log,
        plot_palette = plot_palette,
        plot_x = plot_x,
        plot_y = plot_y,
        plot_custom_labels = plot_custom_labels,
        plot_z = plot_z,
        plot_x_from = plot_x_from,
        plot_x_to = plot_x_to,
        plot_z_lines = plot_z_lines,
        plot_z_value = plot_z_value,
        plot_to_table = plot_to_table,
        test_c = test_c,
        explain = explain,
        inspect_cors = inspect_cors,
        diagram = diagram,
        .interface = "R",
        .caller = "mediation")

    analysis <- pamlmedClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}
