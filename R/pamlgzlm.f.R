#' Generalized Linear Model
#'
#' Power analysis for generalized linear models using eta-squared or R-squared
#' effect-size parameterizations.
#'
#' @param aim The aim of the analysis: \code{n} (default) for sample size,
#'   \code{power} to estimate power, or \code{es} to estimate the minimum
#'   detectable effect size.
#' @param mode Effect size parameterization: \code{"eta"} for eta-squared or
#'   \code{"r2"} for R-squared.
#' @param es Expected effect size.
#' @param df_model Model degrees of freedom.
#' @param power Minimal desired power.
#' @param n Sample size.
#' @param sig.level Type I error rate (significance cut-off or alpha).
#' @param alternative Test direction: \code{"two.sided"} (default) or
#'   \code{"one.sided"}.
#' @param plot_contour Logical; if \code{TRUE}, produce the power contour plot.
#' @param plot_escurve Logical; if \code{TRUE}, produce the power-by-effect-size
#'   curve.
#' @param plot_ncurve Logical; if \code{TRUE}, produce the power-by-sample-size
#'   curve.
#' @param plot_log Logical; if \code{TRUE}, use a log scale where supported in
#'   plots.
#' @param plot_palette Color palette used for plots.
#' @param covs Number of covariates used to compute model degrees of freedom.
#' @param factors Number of factors used to compute model degrees of freedom.
#' @param factors_list Factor definitions used to compute model degrees of freedom.
#' @param covs_order Maximum covariate interaction order.
#' @param factors_order Maximum factor interaction order.
#' @param mixed_order Maximum factor-by-covariate interaction order.
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
#' @param data Optional data frame. Present for interface consistency.
#'
#' @return A results object.
#' @export
pamlgzlm <- function(
    aim = "n",
    mode = "eta",
    es = 0.2,
    df_model = 1,
    power = 0.9,
    n = 20,
    sig.level = 0.05,
    alternative = "two.sided",
    plot_contour = FALSE,
    plot_escurve = FALSE,
    plot_ncurve = FALSE,
    plot_log = FALSE,
    plot_palette = "viridis",
    covs = 0,
    factors = 0,
    factors_list = list(list(var = "factor 1", levels = 0)),
    covs_order = "main",
    factors_order = "main",
    mixed_order = "none",
    plot_x = "none",
    plot_y = "none",
    plot_custom_labels = FALSE,
    plot_z = "none",
    plot_x_from = 0,
    plot_x_to = 0,
    plot_z_lines = 0,
    plot_z_value = list(),
    plot_to_table = FALSE,
    data = NULL
    ) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("pamlgzlm requires jmvcore to be installed (restart may be required)")

    if (is.null(data))
        data <- data.frame()

    eta_es <- if (identical(mode, "eta")) es else 0.2
    eta_df_model <- if (identical(mode, "eta")) df_model else 1
    r2_es <- if (identical(mode, "r2")) es else 0.05
    r2_df_model <- if (identical(mode, "r2")) df_model else 1

    options <- pamlgzlmOptions$new(
        aim = aim,
        mode = mode,
        r2_es = r2_es,
        r2_df_model = r2_df_model,
        eta_es = eta_es,
        eta_df_model = eta_df_model,
        power = power,
        n = n,
        sig.level = sig.level,
        alternative = alternative,
        plot_contour = plot_contour,
        plot_escurve = plot_escurve,
        plot_ncurve = plot_ncurve,
        plot_log = plot_log,
        plot_palette = plot_palette,
        covs = covs,
        factors = factors,
        factors_list = factors_list,
        covs_order = covs_order,
        factors_order = factors_order,
        mixed_order = mixed_order,
        plot_x = plot_x,
        plot_y = plot_y,
        plot_custom_labels = plot_custom_labels,
        plot_z = plot_z,
        plot_x_from = plot_x_from,
        plot_x_to = plot_x_to,
        plot_z_lines = plot_z_lines,
        plot_z_value = plot_z_value,
        plot_to_table = plot_to_table,
        explain = FALSE,
        .interface = "R",
        .caller = "gzlm")

    analysis <- pamlgzlmClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}
