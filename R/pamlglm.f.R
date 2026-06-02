#' General Linear Model
#'
#' Something here
#'
#' @param aim The aim of the analysis: \code{n} (default) for sample size,
#'   \code{power} to estimate power, or \code{es} to estimate the minimum
#'   detectable effect size.
#' @param mode Effect size parameterization: \code{"peta"} for partial
#'   eta-squared, \code{"eta"} for eta-squared, or \code{"beta"} for the
#'   standardized regression coefficient formulation.
#' @param es Expected effect size. It is interpreted as standardized
#'   coefficient for \code{mode="beta"}, partial eta-squared for
#'   \code{mode="peta"}, or eta-squared for \code{mode="eta"}.
#' @param df_effect Effect degrees of freedom for the focal effect. It is used
#'   for \code{mode="peta"} and \code{mode="eta"}. For \code{mode="beta"}, it
#'   is fixed internally to 1.
#' @param df_model Model degrees of freedom.
#' @param r2 Expected model R-squared. It is used for \code{mode="beta"} and
#'   \code{mode="eta"} and ignored for \code{mode="peta"}.
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
#' @param ncp_type What type of non-centrality parameter (NCP) should be used.
#'   The effect size is always transformed into a Cohen's \code{f2}, which is
#'   multiplied by the estimated \code{N} based on the degrees of freedom such
#'   that \code{N=df+edf+1}. \code{model} defines \code{df} as the model
#'   degrees of freedom. This is the method used by \code{G*Power} software.
#'   \code{liberal} uses the effect \code{df}. \code{strict} uses only the
#'   error df (\code{df=0}).
#' @param rx a vector of strings naming the columns from \code{data}
#'   containing the correlations among independent variables
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
#' @param data Optional data frame. It is only used when \code{rx} specifies
#'   variables whose correlations should be read from data.

#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$intro} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$extrainfo} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$issues} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$powertab} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$effectsize} \tab \tab \tab \tab \tab a table \cr
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
#' Tables can be converted to data frames with \code{asDF} or
#' \code{\link{as.data.frame}}. For example:
#'
#' \code{results$powertab$asDF}
#'
#' \code{as.data.frame(results$powertab)}
#'
#' @export
pamlglm <- function(
    aim = "n",
    mode = "peta",
    es = 0.2,
    df_effect = 1,
    df_model = 1,
    r2 = 0.2,
    power = 0.9,
    n = 20,
    sig.level = 0.05,
    alternative = "two.sided",
    plot_contour = FALSE,
    plot_escurve = FALSE,
    plot_ncurve = FALSE,
    plot_log = FALSE,
    plot_palette = "viridis",
    ncp_type = "model",
    rx = NULL,
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
        stop("pamlglm requires jmvcore to be installed (restart may be required)")

    if ( ! missing(rx)) rx <- jmvcore::resolveQuo(jmvcore::enquo(rx))
    if (!missing(rx) && is.null(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(rx), rx, NULL))
    if (is.null(data))
        data <- data.frame()

    b_es <- if (identical(mode, "beta")) es else 0.2
    b_r2 <- if (identical(mode, "beta")) r2 else 0.05
    b_df_model <- if (identical(mode, "beta")) df_model else 1
    v_es <- if (identical(mode, "peta")) es else 0.2
    v_df_model <- if (identical(mode, "peta")) df_model else 1
    v_df_effect <- if (identical(mode, "peta")) df_effect else 1
    e_es <- if (identical(mode, "eta")) es else 0.2
    e_df_model <- if (identical(mode, "eta")) df_model else 1
    e_r2 <- if (identical(mode, "eta")) r2 else 0.2
    e_df_effect <- if (identical(mode, "eta")) df_effect else 1

    options <- pamlglmOptions$new(
        aim = aim,
        mode = mode,
        b_es = b_es,
        b_r2 = b_r2,
        b_df_model = b_df_model,
        v_es = v_es,
        v_df_model = v_df_model,
        v_df_effect = v_df_effect,
        e_es = e_es,
        e_df_model = e_df_model,
        e_r2 = e_r2,
        e_df_effect = e_df_effect,
        power = power,
        n = n,
        sig.level = sig.level,
        alternative = alternative,
        plot_contour = plot_contour,
        plot_escurve = plot_escurve,
        plot_ncurve = plot_ncurve,
        plot_log = plot_log,
        plot_palette = plot_palette,
        covs = 0,
        factors = 0,
        factors_list = list(
                    list(var="factor 1", levels=0)),
        covs_order = "main",
        factors_order = "main",
        mixed_order = "none",
        eta = "-",
        f = "-",
        eta_df_error = 0,
        epsilon = 0,
        omega = 0,
        gpower = 0,
        f2 = 0,
        use = "none",
        ncp_type = ncp_type,
        rx = rx,
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
        .caller = "glm")

    analysis <- pamlglmClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}
