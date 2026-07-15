#' Linear Mixed Models
#'
#' power analysis for linear mixed model
#' 
#' @param aim The aim of the analysis: \code{n} (default) sample size,
#'   \code{power} to estimate power
#' @param find When \code{aim='n'}, indicates whether to find number of clusters \code{find='k'} or number of cases within each cluster \code{find='n'} (default).
#' @param syntax The model to be analysed with possible options
#' @param model A fitted \code{lme4::lmer()} or \code{lme4::glmer()} (binomial) model. When
#'   supplied, \code{syntax}, \code{clusterpars}, \code{categorical}, \code{sigma2} and
#'   \code{model_type} are extracted from it (any of these the user also passes explicitly are
#'   kept instead), and \code{aim} defaults to \code{"power"} for the design observed in the
#'   fitted model (cluster sizes/levels read off the fit). Random effects are extracted as
#'   independent (diagonal) variances -- any estimated covariance between random terms is
#'   dropped, with a warning -- and categorical predictors are simulated with the exact contrast
#'   coding read off the fitted model's own data.frame (see \code{categorical}'s `coding="custom"`
#'   below), so the extracted fixed-effect coefficients remain correctly interpretable. Only
#'   Gaussian and binomial mixed models are supported.
#' @param model_type The model type or family: `linear` (default) for linear mixed model, `logistic` for binomial logistic mixed model. 
#' @param sigma2 Residual variance. Ignored for `model_type="logistic"`
#' @param power Minimal desired power
#' @param sig.level Type I error rate (significance cut-off or alpha)
#' @param  Number of repetitions for Monte Carlo method
#' @param algo The algorithm to use: `mc` (default) for Monte Carlo simulation, `raw` for raw approximation based on Chi-squared (fast but not very accurate)
#' @param mcR Number of repetitions for Monte Carlo method
#' @param parallel Logical: should parallel computing be used for the Monte
#'   Carlo method
#' @param clusterpars A named list of the form `list(cluster1=c(n=n1,k=k1))`, where `cluster1` is the name of the clustering variable
#' in the model, `n1` is the expcted number of cases within each cluster, and `k1` is the expcted number of clusters. if \code{aim=n}, `n1` is
#' used as starting point for sample size. If \code{aim=clusters}, `k1` is used as starting point for number of clusters.
#' @param categorical A named list specifying which variable is categorical. Each entry can be
#'   either a bare number of levels, `list(varname=x)`, which uses `deviation` (sum-to-zero)
#'   coding, or `list(varname=list(levels=x, coding=c))`, which additionally selects a contrast
#'   coding scheme `c`: `deviation` (default), `simple`, `dummy`, `difference`, `helmert`,
#'   `repeated`, or `polynomial`. `coding="custom"` together with `contrasts=<matrix>` (an x by
#'   x-1 numeric contrast matrix, e.g. as returned by `contr.sum()`/`contr.treatment()`/etc.)
#'   assigns that matrix verbatim; this is how `model=` extracts a fitted model's own coding, and
#'   can also be supplied directly. Any variable in the model not mentioned in `categorical` is
#'   assumed to be numeric.
#' @param seed the seed for Monte Carlo simulations, default=42.
#' @param run TRUE (default) run the simulations, otherwise print out the model without results
#' @param verbose (Boolean) `getOption("pamlj.messages")` (default). Print out updates of the simulation steps. 
#' @param ... Used for internal purposes
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$intro} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$extrainfo} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$issues} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$initnotes} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$infotab} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$powertab} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$plotnotes} \tab \tab \tab \tab \tab a html \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$infotab$asDF}
#'
#' \code{as.data.frame(results$infotab)}
#'
#' @export
pamlmixed <- function(
    aim = "n",
    find = "n",
    syntax = NULL,
    model = NULL,
    clusterpars = list(),
    categorical = list(),    
    model_type= "linear",
    sigma2 = 1,
    power = 0.9,
    sig.level = 0.05,
    algo="mc",
    mcR = 500,
    parallel = TRUE,
    set_seed = FALSE,
    seed = 42,
    run=TRUE,
    verbose=getOption("pamlj.messages"),
    ...
    ) {
  
  if ( ! requireNamespace("jmvcore", quietly=TRUE))
    stop("pamlmixed requires jmvcore to be installed (restart may be required)")

  ## `model`: a fitted lme4 fit, extracted into the same syntax/options the
  ## user would otherwise type by hand (see .mixed_from_fit(), R/S3_mixed.R).
  ## Arguments the user also passed explicitly are kept; the aim defaults to
  ## "power" (evaluated at the design observed in the fit) unless overridden.
  if (!is.null(model)) {
      ext <- .mixed_from_fit(model)
      if (missing(syntax))      syntax      <- ext$syntax
      if (missing(clusterpars)) clusterpars <- ext$clusterpars
      if (missing(categorical)) categorical <- ext$categorical
      if (missing(sigma2))      sigma2      <- ext$sigma2
      if (missing(model_type))  model_type  <- ext$model_type
      if (missing(aim))         aim         <- "power"
      if (length(ext$warnings) > 0 && isTRUE(verbose))
          for (w in ext$warnings) message(w)
  }

  if (is.null(syntax))
     stop("Please specify a model with expected coefficients with the parameter `syntax`, ",
          "or pass a fitted lme4 model with `model`")

  pamlj_messages<-getOption("pamlj.messages")
  options("pamlj.messages"=verbose)
  
  
  ## get some info to pass to   pamlmixedClass
  modelobj    <-  try_hard(syntax_digest(syntax))
  if (!isFALSE(modelobj$error)) stop("Model formula not correct:" %+% modelobj$error)
  synmodel <- modelobj$obj   # digested syntax model (distinct from the `model` argument: a fitted lme4 fit)
  ## check the model syntax, functions used are in S3_mixed.R

  ### build var_type out of defaults and categorical option (which is not in jamovi)
  var_type<-lapply(synmodel$varnames, function(x) list(name=x,type="continuous",levels="---"))
  names(var_type)<-synmodel$varnames
  ## categorical[[x]] is either a bare level count (backward-compatible shorthand,
  ## defaults to "deviation" coding) or list(levels=, coding=, contrasts=) for an
  ## explicit coding scheme / a verbatim custom contrast matrix (see .mixed_from_fit()
  ## and .mixed_contrast_matrix(), R/S3_mixed.R)
  for (x in names(categorical)) {
    cx <- categorical[[x]]
    if (is.list(cx)) {
      contr <- cx$contrasts
      if (!is.null(contr) && is.matrix(contr)) contr <- .mixed_encode_contrasts(contr)
      var_type[[x]]<-list(name=x, type="categorical", levels=as.character(cx$levels),
                           coding=if (is.null(cx$coding)) "deviation" else cx$coding,
                           contrasts=if (is.null(contr)) "" else contr)
    } else {
      var_type[[x]]<-list(name=x, type="categorical", levels=as.character(cx),
                           coding="deviation", contrasts="")
    }
  }

  # fix clusterpars that in jamovi has a name element
  try_hard(
  clusterpars<-lapply(names(clusterpars), function(x) {
    pars<-clusterpars[[x]]
    pars$name<-x
    if (!("n" %in% names(pars))) pars$n <- NA
    if (!("k" %in% names(pars))) pars$k <- NA
    pars
  })
  )
  if (model_type=="logistic") sigma2=1
  ## standard stuff
  .interface = "R"
  .caller = "pamlmixed"
  ## deal with extra
  args<-list(...)
  .info<-FALSE
  if (".info" %in% names(args)) .info<-args$.info 
  
  ### now we have it, let's go
  options <- pamlmixedOptions$new(
    aim = aim,
    find= find,
    code=syntax,
    model_type=model_type,
    clusterpars = clusterpars,
    var_type = var_type,
    sigma2 = sigma2,
    power = power,
    sig.level = sig.level,
    algo=algo,
    mcR = mcR,
    parallel = parallel,
    set_seed = set_seed,
    seed = seed,
    .interface = .interface,
    .caller = .caller,
    .info   = .info,
    .run=run
    )
  
  analysis <- pamlmixedClass$new(
    options = options,
    data = data)

  if (run)  
    analysis$run()
  else
    analysis$init()
  
  a<-analysis$results
  options("pamlj.messages"=pamlj_messages)
  return(a)
}

