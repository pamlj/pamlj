#' Linear Mixed Models
#'
#' power analysis for linear mixed model
#' 
#' @param aim The aim of the analysis: \code{n} (default) sample size,
#'   \code{power} to estimate power
#' @param find When \code{aim='n'}, indicates whether to find number of clusters \code{find='k'} or number of cases within each cluster \code{find='n'} (default). 
#' @param syntax The model to be analysed with possible options
#' @param sigma2 Residual variance
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
#' @param categorical A named list of the form `list(varname1=x1,varname2=x2)`, specifying which variable is categorical and the number of levels (x). 
#'   Any variable in the model not mentioned in `categorical ` is assumed to be numeric.
#' @param seed the seed for Monte Carlo simulations, default=42.
#' @param run TRUE (default) run the simulations, otherwise print out the model without results
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
    clusterpars = list(),
    categorical = list(),    
    sigma2 = 1,
    power = 0.9,
    sig.level = 0.05,
    algo="mc",
    mcR = 500,
    parallel = TRUE,
    set_seed = FALSE,
    seed = 42,
    run=TRUE,
    ...
    ) {
  
  if ( ! requireNamespace("jmvcore", quietly=TRUE))
    stop("pamlmixed requires jmvcore to be installed (restart may be required)")
  
  if (is.null(syntax))
     stop("Please speficy a model with expected coefficient with the parameter `syntax`")

  ## get some info to pass to   pamlmixedClass
  model_line <- get_regression_lines(syntax)[[1]]
  modelobj    <-  try_hard(decompose_mixed_formula(model_line))
  if (!isFALSE(modelobj$error)) stop("Model formula not correct:" %+% modelobj$error)
  .model <- modelobj$obj
  ## check the model syntax, functions used are in S3_mixed.R
  fixed<-.model$fixed
  vars<-fixed$terms[fixed$terms!="1"]

  ### build var_type out of defaults and categorical option (which is not in jamovi)
  var_type<-lapply(vars, function(x) list(name=x,type="continuous",levels="---"))
  names(var_type)<-vars
  for (x in names(categorical)) var_type[[x]]<-list(name=x,type="categorical",levels=categorical[[x]])

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
  
  analysis$results
}

