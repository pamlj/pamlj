#' Mediation
#'
#' Something here
#' 
#' @param aim The aim of the analysis: \code{n} (default) for sample size,
#'   \code{power} to estimate power, \code{es} for effect size (correlation)
#' @param mode Mediation model mode: \code{"medsimple"} for simple mediation
#'   with paths \code{a}, \code{b}, and \code{cprime}, or \code{"medcomplex"}
#'   for multiple/serial mediator models.
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
#' @param power Minimal desired power
#' @param n Sample size
#' @param sig.level Type I error rate (significance cut-off or alpha)
#' @param alternative Test direction: \code{"two.sided"} (default) or
#'   \code{"one.sided"}.
#' @param test Mediation test method: \code{"sobel"}, \code{"joint"} for
#'   joint-significance, or \code{"mc"} for Monte Carlo simulation.
#' @param mcR Number of simulation replications used by the Monte Carlo and
#'   simulation-based joint methods.
#' @param parallel Logical; if \code{TRUE}, use parallel computation for
#'   simulation-based methods when available.
#' @param set_seed Logical; if \code{TRUE}, use the value in \code{seed} to
#'   make simulations reproducible.
#' @param seed Random seed used when \code{set_seed=TRUE}.
#' @param table_pwbyn Logical; if \code{TRUE}, produce the "Power by Sample
#'   size" table.
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
    power = 0.9,
    n = 100,
    sig.level = 0.05,
    alternative = "two.sided",
    test = "sobel",
    mcR = 1000,
    parallel = FALSE,
    set_seed = FALSE,
    seed = 42,
    table_pwbyn = TRUE,
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
    diagram = TRUE) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("pamlmed requires jmvcore to be installed (restart may be required)")


    options <- pamlmedOptions$new(
        aim = aim,
        mode = mode,
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
        diagram = diagram,
        .interface = "R",
        .caller = "mediation")

    analysis <- pamlmedClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}
