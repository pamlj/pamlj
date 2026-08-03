## GZLM-specific S3 methods.
## These methods are caller-qualified so they do not override GLM methods.

.checkdata.gzlmeta <- function(obj) {

      obj$data <- data.frame(n = obj$options$n,
                       es = as.numeric(obj$options$eta_es),
                       power = obj$options$power,
                       df    = obj$options$eta_df,
                       sig.level = obj$options$sig.level)
      .checkdata_gzlm(obj)
   }

.checkdata.gzlmr2 <- function(obj) {

  obj$data <- data.frame(n = obj$options$n,
                         es = as.numeric(obj$options$r2_es),
                         power = obj$options$power,
                         df    = obj$options$r2_df,
                         sig.level = obj$options$sig.level)
  
  .checkdata_gzlm(obj)
}

.checkdata_gzlm<-function(obj) {  
  
  obj$data[[obj$aim]] <- NULL
  obj$info$model_type  <- obj$options$model_type
  obj$info$letter<-switch(obj$mode,
                          eta=letter_eta2,
                          r2=letter_r2,
                          stop("no mode defined")
                          )
    
  obj$info$esmax       <- .999999
  obj$info$esmin       <- .001
  obj$info$eslbound    <- 0
  obj$info$loges       <- function(x) x < .05
  obj$info$nmin        <- obj$data$df + 2
  obj$info$logy        <- TRUE
  
  if (is.something(obj$data$es)) {
    if (obj$data$es < 0)
      obj$stop("Eta-squared value cannot be less than 0.")
    if (obj$data$es > obj$info$esmax)
      obj$stop("Eta-squared value cannot be more than " %+% obj$info$esmax)
  }
  
  if (is.something(obj$data$df)) {
    if (obj$data$df < 1)
      obj$stop("Degrees of freedom cannot be less than 1")
  } else {
    obj$stop("GZLM power analysis based on eta-squared requires the expected degrees of freedom of the test")
  }
  ### fix the probabilities
  levels <- obj$options$y_levels
  prop <-  obj$options$y_prop
  obj$info$prob<-switch(obj$info$model_type,
                        logistic= {
                          c(prop,1-prop)
                        },
                        multinomial={
                          if (levels<3) obj$stop("Multinomial model requires more than 2 levels in the dependent variable")
                          y0<-prop
                          y1<-(1-prop)/(levels-1)
                          obj$data$df<-obj$data$df*(levels-1)
                          c(y0,rep(y1,(levels-1)))
                        },
                        ordinal={
                          if (levels<3) obj$stop("Multinomial model requires more than 2 levels in the dependent variable")
                          y0<-prop
                          y1<-(1-prop)/(levels-1)
                          c(y0,rep(y1,(levels-1)))
                        }
  )
}

.powervector.gzlm <- function(obj, data) {

      aim <- required_param(data)
      data<-clean_args(data,pamlj.gzlm)
      results <- lapply(1:nrow(data), function(i) {
          one <- as.list(data[i, ])
          one$prob <- obj$info$prob
          tryobj <- try_hard(do.call(pamlj.gzlm,one))
          if (!isFALSE(tryobj$error))
            out<-NULL
          else
            out<-tryobj$obj
          out
          })

      results <- as.data.frame(do.call("rbind", results))
      for (i in seq_len(ncol(results))) results[[i]] <- unlist(results[[i]])
      odata <- data[, !names(data) %in% names(results)]
      results <- cbind(odata, results)
      results$n<-ceiling(results$n)
      return(results)
}

.effectsize_init.gzlmr2 <- function(obj) {

   tab <- list()
   ladd(tab) <- list(index = letter_r2, value = obj$data$es)
   ladd(tab) <- list(index = "Model df", value = obj$data$df_model)
   return(tab)
}

.effectsize_run.gzlmr2 <- function(obj) {

   .effectsize_init.gzlmr2(obj)
}

.extrainfo.gzlm <- function(obj) {

  if (!obj$option("explain")) return()

  terms <- ifelse(obj$data$df_model > 1, "terms", "term")
  text <- " <p> Power parameters are computed for a generalized linear model with " %+% obj$data$df_model %+%
          " degrees of freedom, for an effect size " %+% obj$info$letter %+% " = " %+% format5(obj$data$es) %+%
          " and type I error rate set to " %+% obj$data$sig.level %+% ".</p>" %+%
          "<p>The model definition corresponds to " %+% obj$data$df_model %+% " model " %+% terms %+% ".</p>" %+%
          "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim, short = TRUE) %+% "=" %+% format5(obj$data[[obj$aim]]) %+% ".</p>"

  obj$warning <- list(topic = "extrainfo", message = text, head = "info")
}
