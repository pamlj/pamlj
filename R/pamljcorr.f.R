
#' Correlation
#'
#' Compute power parameters for Pearson correlations
#' 
#' @param aim The aim of the analysis: \code{n} (default) for sample size,
#'   \code{power} to estimate power, \code{es} for effect size (correlation)
#' @param es The expected effect size, i.e. the expected correlation
#' @param power Minimal desired power
#' @param n Sample size
#' @param sig.level Type I error rate (significance cut-off or alpha)
#' @param alternative `one.side` or `two.sided` (default)
#' @param plot_contour Produce power contour plot
#' @param plot_escurve Produce power as a function of possible effect size values plot
#' @param plot_ncurve  Produce power as a function of possible sample size values plot
#' @param plot_log .   Use log-scale in plots
#' @param plot_palette Change the palettes of the plots
#' @param plot_x Parameter in the X-axis of custom power parameters plot.
#'   \code{n}, \code{power} or \code{es}, the latest being the effect size.
#' @param plot_y Parameter in the Y-axis of custom power parameters plot.
#'   \code{n}, \code{power} or \code{es}, the latest being the effect size.
#' @param plot_custom_labels Add value labels to the custom power parameters
#'   plot, default FALSE.
#' @param plot_z break down the custom power parameters plot by a parameter.
#'   \code{n}, \code{power}, \code{sig.level} or \code{es} for effect size
#' @param plot_x_from Range for the parameter in the X-axis of custom power
#'   parameters plot. Starting value.
#' @param plot_x_to Range for the parameter in the X-axis of custom power
#'   parameters plot. Ending value.
#' @param plot_z_lines not used in R
#' @param plot_z_value Values of the parameter to break the plot by
#' @param plot_to_table Produce a table of the values plotted in custom power
#'   parameters plot
#' @param .interface Used for internal purposes
#' @param .caller Used for internal purposes
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$intro} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$extrainfo} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$issues} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$powertab} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$powerbyes} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$plotnotes} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$powerContour} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$powerEscurve} \tab \tab \tab \tab \tab an image \cr
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
pamlcorr <- function(
    aim = "n",
    es = 0.5,
    power = 0.9,
    n = 20,
    sig.level = 0.05,
    alternative = "two.sided",
    plot_contour = FALSE,
    plot_escurve = FALSE,
    plot_ncurve = FALSE,
    plot_log = TRUE,
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
    .interface = "R",
    .caller = "correlation") {
  
  if ( ! requireNamespace("jmvcore", quietly=TRUE))
    stop("pamlcorr requires jmvcore to be installed (restart may be required)")
  
  
  options <- pamlcorrOptions$new(
    aim = aim,
    es = es,
    power = power,
    n = n,
    sig.level = sig.level,
    alternative = alternative,
    plot_contour = plot_contour,
    plot_escurve = plot_escurve,
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
    .interface = .interface,
    .caller = .caller)
  
  analysis <- pamlcorrClass$new(
    options = options,
    data = data)
  
  analysis$run()
  
  analysis$results
}


