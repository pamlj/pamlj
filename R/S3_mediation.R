## checkdata:       (required) this prepares all the info required to estimate the power parameters

.checkdata.medsimple <- function(obj) {

      jinfo("Checking data for medsimple")

  

      obj$data             <- data.frame(b=obj$options$b)
      obj$data$a           <- obj$options$a
      betas<-list(a=obj$data$a,b=obj$data$b,"c"=obj$data$cprime)
      test <- check_parameters(betas, fun=function(x) (!is.null(x) && abs(x)<.001), verbose=FALSE)
      if (length(test)>0) obj$stop("Standardized coefficients (absolute value) cannot be smaller than .001. Please correct coefficients: " %+% paste(test,collapse=", "))
      
      obj$data$cprime      <- obj$options$cprime
      betas$cprime <- obj$data$cprime
      test <- check_parameters(betas, fun=function(x) (!is.null(x) && abs(x) >.99), verbose=FALSE)
      if (length(test)>0) obj$stop("Standardized coefficients cannot be larger than .99. Please correct coefficients: " %+% paste(test,collapse=", "))

      if (is.something(obj$data$a)) {
                      obj$data$es<-obj$data$a*obj$data$b
                      obj$info$rxy<-obj$data$a*obj$data$b+obj$data$cprime
      }

      ## define the mediation model as a RAM path matrix A (A[i,j] = path FROM
      ## j TO i) and its implied correlation Sigma. These describe the model and
      ## are handed to pamlj.mediation() by .powervector.mediation().
      A <- matrix(0, 3, 3, dimnames = list(c("X","M","Y"), c("X","M","Y")))
      A["M","X"] <- obj$data$a          # a  : X -> M
      A["Y","M"] <- obj$data$b          # b  : M -> Y
      A["Y","X"] <- obj$data$cprime     # c' : X -> Y (direct)
      obj$info$A     <- A
      obj$info$Sigma <- .mediation.implied_cor(A)

      obj$data$n           <- obj$options$n
      obj$data$sig.level   <- obj$options$sig.level
      obj$data$power       <- obj$options$power
      obj$data$alternative <- obj$options$alternative
      obj$data$test        <- obj$options$test
      obj$data$precise     <- TRUE
      obj$data$parallel    <- obj$options$parallel
      if (obj$data$test == "sobel")
          obj$warning     <-  list(topic="issues",message="Sobel test method is based on power parameters approximation. Please use joint-significance or bootstrap confidence intervals for more accurate results ", head="info")
      if (obj$data$test == "joint")
        obj$warning     <-  list(topic="initnotes",message="Joint-significance power is computed from the distribution of each path coefficient.", head="info")
      if (obj$data$test == "parametric")
        obj$warning     <-  list(topic="initnotes",message="Bootstrap CI power (parametric) is estimated with the Monte Carlo confidence-interval method.", head="info")
      if (obj$data$test == "simulation")
        obj$warning     <-  list(topic="initnotes",message="Bootstrap CI power (simulation) is estimated by simulating datasets; this may take a little longer.", head="wait")
      
      obj$data$R           <- obj$options$mcR

      obj$plots$data       <- obj$data

      obj$data[[obj$aim]]  <- NULL
      obj$info$letter      <- "ME"
      obj$info$esmax       <- .9801
      obj$info$esmin       <-  1e-06
      obj$info$nmin        <-  10
      obj$info$nochecks    <-  "es"
      jinfo("Checking data for medsimple done")
}



## complex mediation now uses the SAME design as medsimple: .checkdata defines
## the model (path matrix A, residual covariance S, implied correlation Sigma)
## and hands it to pamlj.mediation() via .powervector.mediation(). The ONLY
## model-specific part is how A and S are built; every indirect (X -> ... -> Y)
## path is then found and powered generically by the Sobel engine.

.checkdata.medcomplex <- function(obj) {

      jinfo("Checking data for medcomplex")

      if (obj$aim == "es") obj$stop("Finding minumum effect size for complex mediation models is not implemented yet")

      o   <- obj$options
      num <- function(x) suppressWarnings(as.numeric(x))

      ## ---- validate the magnitude of the structural (a/b) coefficients -------
      numbs <- sapply(list(a1=o$a1,b1=o$b1,a2=o$a2,b2=o$b2,a3=o$a3,b3=o$b3), as.numeric)
      test  <- check_parameters(numbs, fun=function(x) (!is.na(x) && abs(x) >.99), verbose=FALSE)
      if (length(test)>0) obj$stop("Standardized coefficients cannot be larger than .99. Please correct coefficients: " %+% paste(test,collapse=", "))
      test  <- check_parameters(numbs, fun=function(x) (!is.na(x) && abs(x)<.001), verbose=FALSE)
      if (length(test)>0) obj$stop("Standardized coefficients (absolute value) cannot be smaller than .001. Please correct coefficients: " %+% paste(test,collapse=", "))

      cprime <- num(o$cprime2)

      ## ---- model definition: the ONLY model-specific part --------------------
      ##  Each branch returns: the variable names, the coefficients that must be
      ##  supplied (`req`, named for the missing-input message), the raw values
      ##  for the path diagram (`plotdata`), and a `build()` returning the RAM
      ##  path matrix A and the residual-covariance S among mediators.
      spec <- switch(o$model_type,
            twomeds = {
                  vars <- c("X","M1","M2","Y")
                  req  <- list("X to M1 (a1)"=num(o$a1),"M1 to Y (b1)"=num(o$b1),
                               "X to M2 (a2)"=num(o$a2),"M2 to Y (b2)"=num(o$b2),
                               "M1-M2 correlation (r12)"=num(o$r12))
                  plotdata <- data.frame(a1=num(o$a1),b1=num(o$b1),a2=num(o$a2),b2=num(o$b2),
                                         r12=num(o$r12),cprime=cprime)
                  build <- function() {
                        A <- matrix(0,4,4,dimnames=list(vars,vars))
                        A["M1","X"]<-num(o$a1); A["M2","X"]<-num(o$a2)
                        A["Y","X"]<-cprime; A["Y","M1"]<-num(o$b1); A["Y","M2"]<-num(o$b2)
                        S <- matrix(0,4,4,dimnames=list(vars,vars))
                        S["M1","M2"]<-S["M2","M1"]<- num(o$r12) - num(o$a1)*num(o$a2)
                        list(A=A,S=S)
                  }
                  list(vars=vars,req=req,plotdata=plotdata,build=build)
            },
            threemeds = {
                  vars <- c("X","M1","M2","M3","Y")
                  req  <- list("X to M1 (a1)"=num(o$a1),"M1 to Y (b1)"=num(o$b1),
                               "X to M2 (a2)"=num(o$a2),"M2 to Y (b2)"=num(o$b2),
                               "X to M3 (a3)"=num(o$a3),"M3 to Y (b3)"=num(o$b3),
                               "M1-M2 Correlation (r12)"=num(o$r12),
                               "M1-M3 Correlation (r13)"=num(o$r13),
                               "M2-M3 Correlation (r23)"=num(o$r23))
                  plotdata <- data.frame(a1=num(o$a1),b1=num(o$b1),a2=num(o$a2),b2=num(o$b2),
                                         a3=num(o$a3),b3=num(o$b3),r12=num(o$r12),r13=num(o$r13),
                                         r23=num(o$r23),cprime=cprime)
                  build <- function() {
                        A <- matrix(0,5,5,dimnames=list(vars,vars))
                        A["M1","X"]<-num(o$a1); A["M2","X"]<-num(o$a2); A["M3","X"]<-num(o$a3)
                        A["Y","X"]<-cprime; A["Y","M1"]<-num(o$b1); A["Y","M2"]<-num(o$b2); A["Y","M3"]<-num(o$b3)
                        S <- matrix(0,5,5,dimnames=list(vars,vars))
                        S["M1","M2"]<-S["M2","M1"]<- num(o$r12) - num(o$a1)*num(o$a2)
                        S["M1","M3"]<-S["M3","M1"]<- num(o$r13) - num(o$a1)*num(o$a3)
                        S["M2","M3"]<-S["M3","M2"]<- num(o$r23) - num(o$a2)*num(o$a3)
                        list(A=A,S=S)
                  }
                  list(vars=vars,req=req,plotdata=plotdata,build=build)
            },
            twoserial = {
                  vars <- c("X","M1","M2","Y")
                  req  <- list("X to M1 (a1)"=num(o$a1),"M1 to Y (b1)"=num(o$b1),
                               "X to M2 (a2)"=num(o$a2),"M2 to Y (b2)"=num(o$b2),
                               "M1 to M2 (d1)"=num(o$d1))
                  plotdata <- data.frame(a1=num(o$a1),b1=num(o$b1),a2=num(o$a2),b2=num(o$b2),
                                         d1=num(o$d1),cprime=cprime)
                  build <- function() {
                        A <- matrix(0,4,4,dimnames=list(vars,vars))
                        A["M1","X"]<-num(o$a1); A["M2","X"]<-num(o$a2); A["M2","M1"]<-num(o$d1)
                        A["Y","X"]<-cprime; A["Y","M1"]<-num(o$b1); A["Y","M2"]<-num(o$b2)
                        list(A=A,S=matrix(0,4,4,dimnames=list(vars,vars)))
                  }
                  list(vars=vars,req=req,plotdata=plotdata,build=build)
            }
      )

      obj$plots$data <- spec$plotdata

      ## ---- which coefficients are still missing? -----------------------------
      missing    <- names(spec$req)[vapply(spec$req, is.na, logical(1))]
      obj$filled <- (length(missing) == 0)

      if (obj$filled) {

            mod     <- spec$build()
            A       <- mod$A
            Sigma   <- .mediation.implied_cor(A, mod$S)
            vars    <- rownames(A)
            outcome <- length(vars)            # Y is the last variable

            if (.mediation.r2(Sigma, A, outcome) > .99)
                  obj$stop("Input coefficients are not feasable. The resulting R-squared are impossible. Please adjust the input values")

            ## every indirect effect of interest is an X -> ... -> Y path; the
            ## label (e.g. "X → M1 → Y") also keys the target map used per row.
            paths  <- Filter(function(ch) ch[1] == 1 && ch[length(ch)] == outcome,
                             .mediation.indirect_paths(A))
            labels <- vapply(paths, function(ch) paste(vars[ch], collapse = " → "), character(1))

            ## one table row per indirect effect: a = first edge (X -> first
            ## mediator), es = product of all edges on the path, b = the rest of
            ## the product (so a * b = es), cprime = the direct X -> Y effect.
            exdata        <- data.frame(effect = labels, stringsAsFactors = FALSE)
            exdata$a      <- vapply(paths, function(ch) A[ch[2], ch[1]], numeric(1))
            exdata$es     <- vapply(paths, function(ch)
                                    prod(mapply(function(i,j) A[i,j], ch[-1], ch[-length(ch)])), numeric(1))
            exdata$b      <- ifelse(exdata$a != 0, exdata$es / exdata$a, 0)
            exdata$cprime <- A[outcome, 1]

            ## stash the model for .powervector.mediation(): A and Sigma are shared
            ## by all effects; `targets` maps each effect label to its node path.
            obj$info$A       <- A
            obj$info$Sigma   <- Sigma
            obj$info$S       <- mod$S    # residual covariances among mediators (for es resize)
            obj$info$targets <- setNames(lapply(paths, function(ch) vars[ch]), labels)
            obj$info$rxy     <- Sigma[1, outcome]    # model-implied X-Y correlation

      } else {

            ## not enough input yet: list what is missing and leave `es` unset so
            ## the downstream common checks skip it (the analysis will not run).
            obj$warning <- list(topic="issues", message=missing, head="info")
            exdata      <- data.frame(effect=NA_character_, a=NA_real_, b=NA_real_,
                                      cprime=cprime, stringsAsFactors=FALSE)
      }

      ## ---- shared parameters (identical to medsimple) ------------------------
      exdata$n           <- o$n
      exdata$sig.level   <- o$sig.level
      exdata$power       <- o$power
      exdata$alternative <- o$alternative
      exdata$test        <- o$test
      exdata$precise     <- TRUE
      exdata$parallel    <- o$parallel
      exdata$R           <- o$mcR
      exdata[[obj$aim]]  <- NULL

      if (o$test == "sobel")
            obj$warning <- list(topic="issues",message="Sobel test method is based on power parameters approximation. Please use joint-significance or bootstrap confidence intervals for more accurate results ", head="info")
      if (o$test == "joint")
            obj$warning <- list(topic="initnotes",message="Joint-significance power is computed from the distribution of each path coefficient.", head="info")
      if (o$test == "parametric")
            obj$warning <- list(topic="initnotes",message="Bootstrap CI power (parametric) is estimated with the Monte Carlo confidence-interval method.", head="info")
      if (o$test == "simulation")
            obj$warning <- list(topic="initnotes",message="Bootstrap CI power (simulation) is estimated by simulating datasets; this may take a little longer.", head="wait")

      ## extradata holds every effect (one per table row); obj$data is a single
      ## representative effect -- the smallest one, the hardest to detect -- used
      ## by the plots and the sensitivity / X-Y power tables.
      obj$extradata <- exdata
      w             <- if (obj$filled) which.min(exdata$es)[1] else 1
      obj$data      <- exdata[w, ]

      obj$info$letter   <- "ME"
      obj$info$esmax    <- .9801
      obj$info$esmin    <- 1e-06
      obj$info$nmin     <- 10
      obj$info$nochecks <- "es"
      jinfo("Checking data for medcomplex done")
}


## powervector:     (required) pass the data, with adjutment, to the lowerlevel power functions
##
## Estimates the power parameters for one or more rows of `data` (one row per
## indirect effect for complex models, a single row for the simple model). For
## each row it: (1) chooses the engine (bootstrap vs the analytic Sobel /
## joint engine), (2) keeps only the scalar columns that engine declares as arguments,
## (3) injects the model matrices A / Sigma built in .checkdata() and, for
## complex models, the target indirect path for that row, and (4) calls the
## engine. The per-row results are stacked back onto the input columns.

.powervector.medsimple <- function(obj, data) .powervector.mediation(obj, data)
.powervector.medcomplex <- function(obj, data) .powervector.mediation(obj, data)

.powervector.mediation <- function(obj,data) {

                 aim<-required_param(data)

                 ## dealing with seed for simulations
                 if (obj$options$set_seed) data$seed=obj$options$seed
                 results<-lapply(1:nrow(data),function(i) {
                     
                     test      <- data$test[i]
                     fun       <- pamlj.mediation
                     
                      one      <- as.list(data[i,])
                      one[]    <- lapply(one, function(x) if (is.factor(x)) as.character(x) else x)
                      one      <- one[!vapply(one, function(x) length(x)==1 && is.na(x), logical(1))]
                      ## pass only the scalar arguments the target power function declares
                      .names   <- intersect(names(one), rlang::fn_fmls_names(fun))
                      one      <- one[.names]
                      ## inject the model matrices built in .checkdata()
                      one$A     <- obj$info$A
                      one$Sigma <- obj$info$Sigma
                      ## [["S"]] (exact), NOT $S: simple mediation has no "S" key and
                      ## $S would partial-match "Sigma", passing the correlation matrix
                      ## as fixed residual covariances and corrupting the implied Sigma.
                      one$S     <- obj$info[["S"]]    # residual (co)variances; NULL for simple
                      ## complex models carry one indirect path per row: select it by effect label
                      if (!is.null(obj$info$targets))
                          one$target <- obj$info$targets[[ as.character(data$effect[i]) ]]
                     tryobj<-try_hard(do.call(fun,one), silent=F)
                     out<-tryobj$obj
                     if (!isFALSE(tryobj$error)) {
                     switch(aim,
                            n = {
                                stop("failed on n: ", tryobj$error)
                               },
                            power={ 
                                stop("failed on power: ", tryobj$error)
                                  },
                            es={ 
                               stop("failed on es: ", tryobj$error)
                               }
                            
                            )
                     }
                     out
                    })
                          

                 results<-as.data.frame(do.call("rbind",results))
               
                 if (nrow(results)>3) results<- na.omit(results)
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 
                .names<-c(names(data)[!names( data) %in% names(results)],names(results))
                 odata<- data[, !names( data) %in% names(results)]
                 results<-cbind(odata,results)
                 names(results)<-.names
                 results$n  <- round(results$n,digits=0)
                 return(results)
  
}



## powertab_init:   (not required) this function produces or format the main table, powertab, before running

## In the R/jamovi table lifecycle, an initialized table may not be rewritten
## during run. Therefore simple mediation initializes powertab with the solved
## row, using the same vectorizer as the run phase.
.powertab_init.medsimple <- function(obj) {

          if (!obj$ok) return()

          tab <- powervector(obj, obj$data)
          attr(tab, "titles") <- list(es = obj$info$letter)
          return(tab)
}

.powertab_init.medcomplex <- function(obj) {

          if (!obj$ok) return()
  
          tab <-  obj$extradata
          attr(tab,"titles")<-list(es=obj$info$letter)  
          return(tab)
          
}



## powertab:        (not required) this function produces or format the main table, powertab, after running

.powertab.medcomplex <- function(obj) {

   tab<-powervector(obj,obj$extradata)
   return(tab)
}


## powerbyes:       this function produces or format the powerbyes table , after the estimation is done
## powerbyen:       this function produces or format the powerbyen table , after the estimation is done
## effectsize_init: (not required) some sub.module requires additional effect size to be computed. This function inits the table

.effectsize_init.medsimple <- function(obj) {

    return(list(
               list(index="ME"),
               list(index=letter_r2 %+% " predicting M"),
               list(index=letter_r2 %+% " predicting Y"),
               list(index=" X-Y correlation (c) ")
                 ))
}


## mediators / outcome of the model, read off the path matrix A (the outcome Y
## is the last variable; every other variable with incoming paths is a mediator)
.medcomplex_endo <- function(A) {
    vars    <- rownames(A)
    outcome <- vars[length(vars)]
    endo    <- vars[rowSums(A != 0) > 0]
    list(meds = setdiff(endo, outcome), outcome = outcome)
}

## row labels of the "Computed Parameters" table: one R-squared per mediator
## equation, then the outcome's R-squared, then the X-Y correlation. Generic:
## the rows follow whatever mediators the model's path matrix A contains.
.effectsize_init.medcomplex <- function(obj) {

    A <- obj$info$A
    if (is.null(A)) return(list())
    parts <- .medcomplex_endo(A)
    items <- lapply(parts$meds, function(m) list(index = letter_r2 %+% " predicting " %+% m))
    items <- c(items,
               list(list(index = letter_r2 %+% " predicting " %+% parts$outcome),
                    list(index = " X-Y correlation (c) ")))
    return(items)
}


## effectsize_run:  (not required) some sub.module requires additional effect size to be computed. This function fills the table

.effectsize_run.medsimple <- function(obj) {

   tab <- list()
   ladd(tab) <- list(value=obj$data$es)
   ladd(tab) <- list(value=obj$data$r2a)
   ladd(tab) <- list(value=obj$data$r2y)
   ladd(tab) <- list(value=obj$info$rxy)
   
   return(tab)
  
}

## fills the "Computed Parameters" table built by .effectsize_init.medcomplex():
## each value is the R-squared of one equation, recovered from the implied
## correlation Sigma, in the same mediator/outcome order; last is the X-Y corr.
.effectsize_run.medcomplex <- function(obj) {

  A     <- obj$info$A
  Sigma <- obj$info$Sigma
  if (is.null(A) || is.null(Sigma)) return(list())
  parts <- .medcomplex_endo(A)
  ## R^2 of each mediator equation, then the outcome equation, then the X-Y correlation
  vals  <- lapply(c(parts$meds, parts$outcome),
                  function(v) list(value = .mediation.r2(Sigma, A, v)))
  vals  <- c(vals, list(list(value = obj$info$rxy)))
  return(vals)
}



## extrainfo:       (better if is there) this function add some extra info to be given to the user when the option "explain" is selected

.extrainfo.mediation <- function(obj) {
 
   if (!obj$option("explain")) return()  
  
   switch (obj$data$test,
           sobel = test <-"for the <b>Sobel test (z-test)</b>",
           joint = test <- "<b> for joint significance test </b> (both a and b significant)",
           parametric = test <- "with <b>bootstrap confidence intervals (parametric Monte Carlo)</b>",
           simulation = test <- "with <b>bootstrap confidence intervals (simulation)</b>"
   )
    infoparms<-list(n="total sample size N=" %+% obj$data$n,
                   power="power equal to " %+% format5(obj$data$power)
                   )

   if (is.null(obj$extradata)) 
            infoparms$es <- "completely standardized effect size " %+% obj$info$letter %+% " = " %+% format5(obj$data$es) %+% " given by a*b=" %+% format5(obj$data$a) %+% "*" %+% format5(obj$data$b)
      else 
            infoparms$es<- "mediated effects" %+% paste(obj$extradata$effect,collapse=", ")
  
   
   infoparms[[obj$aim]]<-NULL

   text<-"<p> Power parameters are computed " %+% test %+% " with " %+%
          paste(infoparms, collapse=", ") %+% 
          " and type I error rate set to " %+% obj$data$sig.level %+% "." %+%
          " The test tests whether the mediated effect is different from zero." %+%
            "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format5(obj$data[[obj$aim]]) %+% "."
   
    if (obj$aim == "es") text <- text %+% " The required X to mediation coefficient (a) is " %+% format5(obj$data$a) %+% " yielding a power equal to " %+%  format5(obj$data$power) %+% "."

     text <- text %+% "</p>"
     
     if (is.something(obj$info$ryxpower)) text <- text %+%
                                                 "<p> Given the results, the test concerning the simple regression between X and Y will have power equal to " %+%
                                                  obj$info$ryxpower %+% 
                                                 "<p>"
     
    obj$warning<-list(topic="extrainfo",message=text,head="info")  

}
