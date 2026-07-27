% Generated from R/pamlgzlm.f.R
\name{pamlgzlm}
\alias{pamlgzlm}
\title{Generalized Linear Model}
\usage{
pamlgzlm(
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
)
}
\arguments{
\item{aim}{The aim of the analysis: \code{n}, \code{power}, or \code{es}.}
\item{mode}{Effect size parameterization: \code{"eta"} or \code{"r2"}.}
\item{es}{Expected effect size.}
\item{df_model}{Model degrees of freedom.}
\item{power}{Minimal desired power.}
\item{n}{Sample size.}
\item{sig.level}{Type I error rate.}
\item{alternative}{Test direction: \code{"two.sided"} or \code{"one.sided"}.}
\item{plot_contour, plot_escurve, plot_ncurve}{Logical plot toggles.}
\item{plot_log}{Logical; use a log scale where supported.}
\item{plot_palette}{Color palette used for plots.}
\item{covs, factors, factors_list, covs_order, factors_order, mixed_order}{Model-definition inputs used to compute model degrees of freedom.}
\item{plot_x, plot_y, plot_custom_labels, plot_z, plot_x_from, plot_x_to, plot_z_lines, plot_z_value, plot_to_table}{Custom power plot options.}
\item{data}{Optional data frame. Present for interface consistency.}
}
\value{A results object.}
\description{
Power analysis for generalized linear models using eta-squared or R-squared effect-size parameterizations.
}
