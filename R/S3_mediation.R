## checkdata:       (required) this prepares all the info required to estimate the power parameters


.checkdata.medmodels <- function(obj) {

  jinfo("Checking data for medmodels")

  ## digest the lm()-style syntax the user typed
  syntaxobj <- try_hard(syntax_digest(obj$options$code))
  if (!isFALSE(syntaxobj$error))
        obj$stop("Model formula not correct: " %+% syntaxobj$error)

 
  ## validate that the syntax describes a recursive mediation model
  parsed <- .mediation.parse_syntax(syntaxobj$obj)
  if (!parsed$ok) {
        obj$warning <- list(topic = "issues", message = parsed$message, head = parsed$head)
        obj$ok      <- FALSE
        return()
  }
  
  ## ---- valid model: build A / Sigma and the per-effect table -------------
  ## medmodels is structurally identical to medcomplex (several indirect
  ## effects, one table row each, all powered by the generic engine). The ONLY
  ## difference is that A and the paths come from the parsed syntax rather than
  ## a fixed model_type, so from here on the population mirrors medcomplex.
  o     <- obj$options
  A     <- parsed$A
  vars  <- parsed$vars

  ## residual correlations among variables, from `cor(v1,v2)=r` commands (e.g.
  ## correlated parallel mediators). These build the disturbance covariance S.
  sres <- .mediation.build_S(A, syntaxobj$obj$commands$cor)
  if (!sres$ok) {
        obj$warning <- list(topic = "issues", message = sres$message, head = "info")
        obj$ok      <- FALSE
        return()
  }
  S     <- sres$S
  Sigma <- suppressWarnings(.mediation.implied_cor(A, S))

  ## the specified correlations must yield a valid (positive-definite) matrix
  ev <- tryCatch(min(eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values),
                 error = function(e) NA_real_)
  if (is.null(Sigma) || any(!is.finite(Sigma)) || is.na(ev) || ev <= 1e-8)
        obj$stop("The specified correlations are not feasable (they imply an impossible ",
                 "covariance matrix). Please adjust the cor() values.")

  ## feasibility: no endogenous equation may imply an impossible R-squared
  endo <- which(rowSums(A != 0) > 0)
  bad  <- vapply(endo, function(i) .mediation.r2(Sigma, A, i) > .99, logical(1))
  if (any(bad))
        obj$stop("Input coefficients are not feasable: the implied R-squared for '",
                 paste(vars[endo[bad]], collapse = "', '"),
                 "' is impossible. Please adjust the coefficients.")

  ## complete indirect effects = simple paths from a source (exogenous: no
  ## incoming path) to a sink (final outcome: no outgoing path), with >= 2 edges.
  sources <- which(rowSums(A != 0) == 0)
  sinks   <- which(colSums(A != 0) == 0)
  paths   <- Filter(function(ch) ch[1] %in% sources && ch[length(ch)] %in% sinks,
                    parsed$paths)
  if (length(paths) == 0) paths <- parsed$paths     # safety net (acyclic models always have one)

  ## one table row per indirect effect: a = first edge (X -> first mediator),
  ## es = product of all edges on the path (a * b), b = the rest of the product,
  ## cprime = the direct X -> Y effect of that path when the model includes it.
  labels        <- vapply(paths, function(ch) paste(vars[ch], collapse = " → "), character(1))
  exdata        <- data.frame(effect = labels, stringsAsFactors = FALSE)
  exdata$a      <- vapply(paths, function(ch) A[ch[2], ch[1]], numeric(1))
  exdata$es     <- vapply(paths, function(ch)
                          prod(mapply(function(i, j) A[i, j], ch[-1], ch[-length(ch)])), numeric(1))
  exdata$b      <- ifelse(exdata$a != 0, exdata$es / exdata$a, 0)
  exdata$cprime <- vapply(paths, function(ch) A[ch[length(ch)], ch[1]], numeric(1))

  ## stash the model for .powervector.mediation(): A / Sigma are shared by all
  ## effects; `targets` maps each effect label to its node path. S holds the
  ## residual covariances from any cor() commands (NULL when none were given);
  ## `cors` keeps the user-specified correlations for the path diagram.
  obj$info$A       <- A
  obj$info$Sigma   <- Sigma
  obj$info$S       <- S
  obj$info$cors    <- sres$cors
  obj$info$targets <- setNames(lapply(paths, function(ch) vars[ch]), labels)
  obj$filled       <- TRUE

  ## ---- sensitivity coefficient (es aim) ----------------------------------
  ## The user can label a path coefficient in the syntax (e.g. `y ~ a*.2*m1`)
  ## and select it for the minimum-detectable-effect search with `test:a`. The
  ## labelled edge is then the one resized; if it is missing or not on any
  ## indirect effect we warn and fall back to the per-effect default.
  obj$info$coef_edges       <- parsed$coef_labels
  obj$info$sensitivity_edge <- NULL
  if (identical(obj$aim, "es")) {
        sel <- unlist(syntaxobj$obj$commands$test)
        if (length(sel) > 0) {
              sel  <- sel[1]
              edge <- parsed$coef_labels[[sel]]
              if (is.null(edge)) {
                    obj$warning <- list(topic = "issues", head = "info",
                          message = paste0("The coefficient label '", sel, "' (test:", sel,
                          ") is not defined in the model syntax. The minimum detectable effect ",
                          "is found for each indirect effect separately."))
              } else {
                    ei      <- match(edge, vars)   # c(to_idx, from_idx)
                    on_path <- any(vapply(paths, function(ch)
                                    any(ch[-length(ch)] == ei[2] & ch[-1] == ei[1]), logical(1)))
                    if (!on_path) {
                          obj$warning <- list(topic = "issues", head = "info",
                                message = paste0("The coefficient '", sel, "' is not part of any ",
                                "indirect effect. The minimum detectable effect is found for each ",
                                "indirect effect separately."))
                    } else {
                          obj$info$sensitivity_edge  <- edge
                          obj$info$sensitivity_label <- sel
                    }
              }
        }
  }

  ## ---- shared parameters (identical to medsimple / medcomplex) -----------
  exdata$n           <- o$n
  exdata$sig.level   <- o$sig.level
  exdata$power       <- o$power
  exdata$alternative <- o$alternative
  exdata$test        <- o$test
  exdata$precise     <- TRUE
  exdata$parallel    <- o$parallel
  exdata$R           <- o$mcR
  ## smallest (hardest-to-detect) effect = representative row; capture its index
  ## BEFORE dropping the aim column (es is removed when aim == "es").
  smallest           <- which.min(exdata$es)[1]
  rep_path           <- paths[[smallest]]
  obj$info$rxy       <- Sigma[rep_path[1], rep_path[length(rep_path)]]   # representative X-Y corr
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
  ## representative effect -- the smallest, hardest to detect -- used by the
  ## plots and the sensitivity / X-Y power tables.
  obj$extradata  <- exdata
  obj$data       <- exdata[smallest, ]
  obj$plots$data <- obj$data

  obj$info$letter   <- "ME"
  obj$info$esmax    <- .9801
  obj$info$esmin    <- 1e-06
  obj$info$nmin     <- 10
  obj$info$nochecks <- "es"
  jinfo("Checking data for medmodels done")
}

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

      ## minimum-detectable-effect (es aim): which coefficient to resize while
      ## solving for the effect. `a` is the X -> M edge (default), `b` is the
      ## M -> Y edge. The option is shared with the complex models, so a value
      ## that is not `a`/`b` falls back to `a` (with a warning under the es aim).
      sel  <- obj$options$sensitivity_coef
      edge <- switch(sel, a = c("M","X"), b = c("Y","M"), NULL)
      if (is.null(edge)) {
            if (identical(obj$aim, "es"))
                  obj$warning <- list(topic = "issues", head = "info",
                        message = paste0("The coefficient '", sel, "' is not a coefficient of the ",
                        "simple mediation model (use 'a' or 'b'). The minimum detectable effect ",
                        "is found by resizing 'a'."))
            edge <- c("M","X"); sel <- "a"
      }
      obj$info$vary_edge         <- match(edge, rownames(A))   # c(to, from) into A
      obj$info$sensitivity_label <- sel

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
                  coef_edges <- list(a1=c("M1","X"), b1=c("Y","M1"),
                                     a2=c("M2","X"), b2=c("Y","M2"))
                  list(vars=vars,req=req,plotdata=plotdata,build=build,coef_edges=coef_edges)
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
                  coef_edges <- list(a1=c("M1","X"), b1=c("Y","M1"),
                                     a2=c("M2","X"), b2=c("Y","M2"),
                                     a3=c("M3","X"), b3=c("Y","M3"))
                  list(vars=vars,req=req,plotdata=plotdata,build=build,coef_edges=coef_edges)
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
                  coef_edges <- list(a1=c("M1","X"), b1=c("Y","M1"),
                                     a2=c("M2","X"), b2=c("Y","M2"),
                                     d1=c("M2","M1"))
                  list(vars=vars,req=req,plotdata=plotdata,build=build,coef_edges=coef_edges)
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
            ## coefficient -> A edge map, for the sensitivity (MDE) analysis: the
            ## user picks one coefficient to resize while solving for the minimum
            ## detectable indirect effect.
            obj$info$coef_edges <- spec$coef_edges

            if (obj$aim == "es") {
                  sel <- obj$options$sensitivity_coef
                  edge <- spec$coef_edges[[sel]]
                  if (is.null(edge) || A[edge[1], edge[2]] == 0)
                        obj$stop("The coefficient '", sel, "' selected for the sensitivity analysis ",
                                 "is not a path of the current model. Please choose one of: ",
                                 paste(names(spec$coef_edges), collapse = ", "))
            }

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
      ## smallest (hardest-to-detect) effect = representative row; capture its
      ## index BEFORE dropping the aim column (es is removed when aim == "es").
      smallest           <- if (obj$filled && !is.null(exdata$es)) which.min(exdata$es)[1] else 1
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
      obj$data      <- exdata[smallest, ]

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

## Complex models: the n / power aims are independent across indirect effects and
## go through the generic per-row engine. The es aim (minimum detectable effect)
## is NOT independent -- a single user-chosen coefficient is resized and every
## effect is recomputed at that value -- so it is handled by a dedicated orchestrator.
.powervector.medcomplex <- function(obj, data) {
      if (identical(required_param(data), "es"))
            return(.powervector.medcomplex_mde(obj, data))
      .powervector.mediation(obj, data)
}

## Free (syntax) models: when the user labelled a coefficient and selected it
## with `test:<label>`, the es aim resizes that one edge and recomputes every
## affected effect -- the same coordinated search complex models use. Without a
## valid selection the es aim falls through to the per-effect default (each
## effect's own first edge is resized independently).
.powervector.medmodels <- function(obj, data) {
      if (identical(required_param(data), "es") && !is.null(obj$info$sensitivity_edge))
            return(.powervector.medcomplex_mde(obj, data))
      .powervector.mediation(obj, data)
}

## ----------------------------------------------------------------------------
## Minimum detectable indirect effect for complex models.
##
## The user selects ONE path coefficient (a1, b1, d1, ...) in the Sensitivity
## panel. We resize that single coefficient until the hardest-to-detect indirect
## effect that travels through it reaches the target power at the given N. The
## residual covariances S (mediator correlations) are held fixed during search.
##
## .medcomplex_mde() does the resize and returns the solved model plus the
## metadata (the varied edge, the affected paths, a power engine) that both the
## main table (.powervector.medcomplex_mde) and the Power-by-effect-size table
## (.powerbyes.medcomplex) reuse.
## ----------------------------------------------------------------------------
.medcomplex_mde <- function(obj, n_val, target_power) {

      A       <- obj$info$A
      S       <- obj$info[["S"]]   # exact match: `$S` partial-matches `Sigma` when S is NULL
      vars    <- rownames(A)
      targets <- obj$info$targets

      ## chosen coefficient -> A edge. Free (syntax) models resolve it from a
      ## symbolic label (obj$info$sensitivity_edge, set in .checkdata.medmodels);
      ## complex models use the GUI dropdown (validated in .checkdata.medcomplex).
      edge_names <- if (!is.null(obj$info$sensitivity_edge)) obj$info$sensitivity_edge
                    else obj$info$coef_edges[[ obj$options$sensitivity_coef ]]
      vary <- match(edge_names, vars)  # c(to, from)
      all_chains <- lapply(targets, function(v) match(v, vars))

      ## affected = indirect paths that traverse the chosen edge. Shrinking the
      ## coefficient lowers every affected effect's power monotonically, so we
      ## resize it until the WEAKEST (lowest-power) affected effect just reaches
      ## the target power -- then every affected effect reaches at least the target.
      traverses <- function(ch) any(ch[-length(ch)] == vary[2] & ch[-1] == vary[1])
      affected  <- Filter(traverses, all_chains)
      es_of     <- function(ch) prod(mapply(function(i, j) A[i, j], ch[-1], ch[-length(ch)]))
      rep_chain <- affected[[ which.min(abs(vapply(affected, es_of, numeric(1)))) ]]

      test   <- obj$options$test
      alpha  <- obj$options$sig.level
      alt    <- obj$options$alternative
      ## Monte Carlo power is a random function, so resizing the coefficient by
      ## root-finding needs a frozen objective: with common random numbers (a
      ## fixed seed re-applied before every evaluation) the simulated power is a
      ## smooth, deterministic function of the coefficient and uniroot cannot fail
      ## to bracket the target or drift run to run. The same engine is reused to
      ## recompute the effects at the solved value, so the reported power matches
      ## the target instead of being a fresh noisy draw. Use the user's seed when
      ## supplied, else a fixed internal seed.
      is_mc  <- test %in% c("parametric", "simulation")
      seed   <- if (isTRUE(obj$options$set_seed)) obj$options$seed
                else if (is_mc) 20240101L else NULL
      engine <- .mediation.power_dispatch(test, alpha, alt, R = obj$options$mcR,
                                          parallel = obj$options$parallel, seed = seed)
      ## minimum power across all affected effects, evaluated on the trial model
      min_affected <- function(eng) function(Amat, Sig, nn)
            min(vapply(affected, function(ch) eng(Amat, Sig, nn, ch), numeric(1)))

      ## Monte Carlo tests are noisy and costly, so seed from a joint-significance pilot.
      if (test %in% c("parametric", "simulation")) {
            pilot_engine <- .mediation.power_dispatch("joint", alpha, alt)
            pilot  <- .mediation.solve_mde(A, rep_chain, S, n_val, target_power,
                                           min_affected(pilot_engine), vary_edge = vary)
            solved <- .mediation.solve_mde(A, rep_chain, S, n_val, target_power, min_affected(engine),
                                           seed_mag = abs(pilot$A[vary[1], vary[2]]), vary_edge = vary)
      } else {
            solved <- .mediation.solve_mde(A, rep_chain, S, n_val, target_power, min_affected(engine),
                                           vary_edge = vary)
      }

      list(A = solved$A, S = S, vars = vars, targets = targets, vary = vary,
           rep_chain = rep_chain, engine = engine, method = solved$method)
}

## Solved value of the varied coefficient, and the representative indirect effect.
.medcomplex_mde_coef <- function(mde) mde$A[mde$vary[1], mde$vary[2]]
.medcomplex_mde_es   <- function(mde)
      prod(mapply(function(i, j) mde$A[i, j], mde$rep_chain[-1], mde$rep_chain[-length(mde$rep_chain)]))

.powervector.medcomplex_mde <- function(obj, data) {

      mde          <- .medcomplex_mde(obj, data$n[1], data$power[1])
      Asolved      <- mde$A
      Sigma_solved <- .mediation.safe_implied_cor(Asolved, mde$S)
      alpha        <- obj$options$sig.level
      chain_of     <- function(label) match(mde$targets[[ as.character(label) ]], mde$vars)

      ## recompute every requested effect at the solved model
      results <- lapply(seq_len(nrow(data)), function(i) {
            ch <- chain_of(data$effect[i])
            pw <- mde$engine(Asolved, Sigma_solved, data$n[1], ch)
            .mediation.result(Asolved, Sigma_solved, ch, data$n[1], pw, alpha, mde$method)
      })

      results <- as.data.frame(do.call("rbind", results))
      for (i in seq_len(ncol(results))) results[[i]] <- unlist(results[[i]])
      .names  <- c(names(data)[!names(data) %in% names(results)], names(results))
      odata   <- data[, !names(data) %in% names(results), drop = FALSE]
      results <- cbind(odata, results)
      names(results) <- .names
      results$n <- round(results$n, digits = 0)
      results
}

## When the effect-size aim cannot reach the requested power at the given N,
## solve_mde() caps at the peak of the (unimodal) power curve and flags the row
## with method "powmax". The result is correct -- it is the largest detectable
## indirect effect -- but the reported power sits below the request, so without a
## message it looks as if changing the desired power does nothing. Surface a clear
## warning naming the requested power and the maximum that is actually achievable.
## Called once per run from the table builders (.powertab[_init]) on the full
## set of effects, so the message is emitted a single time and lists every
## affected effect. The by-effect-size table deliberately probes unreachable
## bands (precise = FALSE) and must stay quiet, hence the precise gate below.
.mediation.feasibility_warning <- function(obj, results, aim) {
      if (!isTRUE(results$precise[1])) return(invisible())
      if (!identical(aim, "es")) return(invisible())

      letter <- obj$info$letter
      want   <- format5(obj$options$power)

      ## "balanced": the requested power was unreachable by resizing the chosen
      ## coefficient alone, so all coefficients of the path were set to a common
      ## value that does reach it. Tell the user which value / effect this implies.
      bal <- which(results$method == "balanced")
      if (length(bal) > 0) {
            lines <- vapply(bal, function(i) {
                  eff <- if (!is.null(results$effect)) paste0(" for ", results$effect[i], ",") else ""
                  paste0(eff, " all path coefficients set to ", format5(results$a[i]),
                         " give an indirect effect of ", letter, " = ", format5(results$es[i]))
            }, character(1))
            message <- paste0("The desired power (", want, ") cannot be reached at N = ",
                              round(results$n[1]), " by resizing the selected coefficient alone ",
                              "(its other path coefficients cap the achievable power). ",
                              "The balanced solution is shown instead:",
                              paste(lines, collapse = ";"), ".")
            obj$warning <- list(topic = "issues", message = message, head = "info")
      }

      ## "powmax": even the balanced solution could not reach the target.
      hit <- which(results$method == "powmax")
      if (length(hit) == 0) return(invisible())
      lines <- vapply(hit, function(i) {
            eff <- if (!is.null(results$effect)) paste0(" for ", results$effect[i], ",") else ""
            paste0(eff, " the largest detectable effect (", letter, " = ", format5(results$es[i]),
                   ") yields a maximum power of ", format5(results$power[i]))
      }, character(1))
      message <- paste0("The desired power (", want, ") cannot be reached at N = ",
                        round(results$n[1]), ":", paste(lines, collapse = ";"),
                        ". Increase the sample size to reach the desired power.")
      obj$warning <- list(topic = "issues", message = message, head = "warning")
      invisible()
}

.powervector.mediation <- function(obj,data) {

                 aim<-required_param(data)

                 ## The main run overwrites obj$data with the solved pamlj.mediation
                 ## result, which carries no `test` column. Tables (powerbyn,
                 ## powerbyes) re-vectorize from that data, so restore the selected
                 ## test here or the engine silently falls back to its sobel default.
                 if (is.null(data$test)) data$test <- obj$options$test

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
                      ## es aim: the coefficient to resize (simple model lets the user
                      ## pick a vs b). [["vary_edge"]] exact-match; NULL -> solve_mde default.
                      one$vary_edge <- obj$info[["vary_edge"]]
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
          ## the main (precise) table is the single point that warns about an
          ## unreachable MDE -- emitted here, on the full result set, exactly once
          ## per run (auxiliary powervector calls for plots / text stay quiet).
          .mediation.feasibility_warning(obj, tab, obj$aim)
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
   ## single, authoritative MDE-feasibility warning over every indirect effect
   ## (medcomplex and medmodels): emitted once here, not per powervector call.
   .mediation.feasibility_warning(obj, tab, obj$aim)
   return(tab)
}


## ----------------------------------------------------------------------------
## Free (syntax) models reuse medcomplex's table machinery: several indirect
## effects, one row each. The generic engine handles every aim per row -- es is
## the per-path minimum detectable effect (like medsimple), not the coordinated
## single-coefficient resize medcomplex does -- so .powervector / .powerbyes
## dispatch through to the mediation / medsimple methods via the class chain
## c("medmodels","mediation",...). Only the multi-row tables need an alias.
## ----------------------------------------------------------------------------
.powertab_init.medmodels <- .powertab_init.medcomplex
.powertab.medmodels      <- .powertab.medcomplex

## Power-by-effect-size. When the user selected a coefficient (test:<label>),
## resize THAT edge and report the bands like the complex model; otherwise fall
## back to the simple-model behaviour (resize the representative effect's first
## edge). (Wrapper, not an alias: the medsimple/medcomplex helpers are defined
## further down this file.)
.powerbyes.medmodels <- function(obj) {
      if (is.null(obj$info$sensitivity_edge))
            return(.powerbyes.medsimple(obj))

      powers <- c(.5, .8, .95)
      n_val  <- obj$data$n
      mdes   <- lapply(powers, function(p) .medcomplex_mde(obj, n_val, p))
      es     <- vapply(mdes, .medcomplex_mde_es,   numeric(1))
      coef   <- vapply(mdes, .medcomplex_mde_coef, numeric(1))
      .power_es_bands(es, coef, obj$info$letter, obj$info$sensitivity_label)
}

## R-squared of each endogenous equation (every variable with incoming paths),
## recovered from the implied correlation Sigma, then the representative X-Y
## correlation. Order-independent: it does not assume the outcome is last.
.effectsize_init.medmodels <- function(obj) {
    A <- obj$info$A
    if (is.null(A)) return(list())
    endo  <- rownames(A)[rowSums(A != 0) > 0]
    items <- lapply(endo, function(v) list(index = letter_r2 %+% " predicting " %+% v))
    c(items, list(list(index = " X-Y correlation (c) ")))
}

.effectsize_run.medmodels <- function(obj) {
    A     <- obj$info$A
    Sigma <- obj$info$Sigma
    if (is.null(A) || is.null(Sigma)) return(list())
    endo <- which(rowSums(A != 0) > 0)
    vals <- lapply(endo, function(i) list(value = .mediation.r2(Sigma, A, i)))
    c(vals, list(list(value = obj$info$rxy)))
}


## powerbyes:       this function produces or format the powerbyes table , after the estimation is done
## powerbyen:       this function produces or format the powerbyen table , after the estimation is done
## effectsize_init: (not required) some sub.module requires additional effect size to be computed. This function inits the table

## ----------------------------------------------------------------------------
## Power by effect size (sensitivity) table.
##
## Same logic as "Power by sample size": with N and every other parameter fixed
## at the main analysis, resize the coefficient (a for simple models, the
## Sensitivity-panel coefficient for complex ones) to the value that yields power
## .5 / .8 / .95, and report, per power band, the resulting indirect effect and
## the coefficient value. .power_es_bands() turns three thresholds into the four
## bands that init_powerbyes() labels.
## ----------------------------------------------------------------------------
.power_es_bands <- function(es, coef, es_symbol, coef_symbol) {
      leq <- greek_vector["leq"]
      e   <- format(round(es, 3))
      cf  <- format(round(coef, 3))
      band <- function(v, sym) c(paste('0 <', sym, leq, v[1]),
                                 paste(v[1], '<', sym, leq, v[2]),
                                 paste(v[2], '<', sym, leq, v[3]),
                                 paste(sym, '>', v[3]))
      eb <- band(e, es_symbol); cb <- band(cf, coef_symbol)
      lapply(seq_len(4), function(i) list(es = eb[i], coef = cb[i]))
}

.powerbyes.medsimple <- function(obj) {
      powers     <- c(.5, .8, .95)
      data       <- obj$data
      data$power <- NULL
      suppressWarnings(dd <- as.data.frame(cbind(power = powers, data)))
      dd$es      <- NULL
      dd$precise <- FALSE
      res        <- powervector(obj, dd)     # es aim: resize the chosen edge at the fixed N
      if (any(is.na(res$es)))
            warning("Some effect sizes cannot be computed given the input parameters.")
      ## report the coefficient that was actually resized (a by default, b when chosen)
      lab  <- obj$info$sensitivity_label
      coef <- if (identical(lab, "b")) res$b else res$a
      .power_es_bands(res$es, coef, obj$info$letter, if (is.null(lab)) "a" else lab)
}

.powerbyes.medcomplex <- function(obj) {
      powers <- c(.5, .8, .95)
      n_val  <- obj$data$n
      mdes   <- lapply(powers, function(p) .medcomplex_mde(obj, n_val, p))
      es     <- vapply(mdes, .medcomplex_mde_es,   numeric(1))
      coef   <- vapply(mdes, .medcomplex_mde_coef, numeric(1))
      .power_es_bands(es, coef, obj$info$letter, obj$options$sensitivity_coef)
}

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


#### helpers for syntax


## ============================================================================
##  .mediation.has_cycle() : TRUE if the directed graph encoded in the RAM path
##  matrix A (A[i, j] != 0 means j -> i) contains a directed cycle. Mediation
##  models must be recursive (acyclic).
## ============================================================================

.mediation.has_cycle <- function(A) {
  p <- nrow(A)
  if (p == 0) return(FALSE)
  succ  <- lapply(seq_len(p), function(u) which(A[, u] != 0))   # u -> these
  state <- integer(p)        # 0 = unvisited, 1 = in progress, 2 = done
  found <- FALSE
  visit <- function(u) {
    if (found) return(invisible())
    state[u] <<- 1L
    for (v in succ[[u]]) {
      if (state[v] == 1L) { found <<- TRUE; return(invisible()) }
      if (state[v] == 0L) visit(v)
    }
    state[u] <<- 2L
  }
  for (u in seq_len(p)) if (state[u] == 0L) visit(u)
  found
}

## ============================================================================
##  .mediation.parse_syntax() : validate that the digested model syntax (a set
##  of lm()-style equations) describes a recursive mediation model and, if so,
##  build its RAM path matrix A. `model` is what syntax_digest() returns: either
##  a single `syntax_formula` (one equation) or a list of them (one per
##  equation) plus a trailing `$commands` element.
##
##  Returns a list with `ok`; on failure also `message`/`head` (a specific,
##  user-facing reason); on success also `A` (path matrix, A[i,j] = path FROM j
##  TO i), `vars`, and `paths` (the indirect chains from .mediation.indirect_paths).
## ============================================================================

.mediation.parse_syntax <- function(model) {
  
  fail <- function(msg, head = "info") list(ok = FALSE, head = head, message = msg)
  
  ## ---- normalise to a list of equations -----------------------------------
  if ("syntax_formula" %in% class(model))
    eqs <- list(model)
  else
    eqs <- Filter(function(x) "syntax_formula" %in% class(x), model)
  
  if (length(eqs) == 0)
    return(fail("Please insert a mediation model in the syntax."))
  
  ## ---- per-equation checks; collect directed edges (from -> to) -----------
  edges    <- list()                     # each: list(from=, to=, value=)
  outcomes <- character(0)               # LHS of each equation
  labels   <- list()                     # symbolic coefficient label -> c(to, from)

  for (eq in eqs) {
    
    lhs <- eq$lhs
    if (is.null(lhs) || !nzchar(trimws(lhs)))
      return(fail("Every equation must have an outcome on the left of '~'."))
    lhs <- trimws(lhs)
    
    if (lhs %in% outcomes)
      return(fail(paste0("Variable '", lhs, "' is the outcome of more than one equation. ",
                         "Combine its predictors into a single equation.")))
    outcomes <- c(outcomes, lhs)
    
    if (isFALSE(attr(eq$terms, "unique")))
      return(fail(paste0("The equation for '", lhs, "' lists the same predictor more than once.")))
    
    terms <- eq$terms
    coefs <- eq$coefs
    used  <- 0L
    
    for (i in seq_along(terms)) {
      term <- trimws(terms[i])
      if (term %in% c("0", "1") || !nzchar(term)) next        # intercept token: ignore
      if (grepl(":", term, fixed = TRUE) || grepl("\\[", term))
        return(fail(paste0("Term '", term, "' in the equation for '", lhs,
                           "' is an interaction or categorical term, which is not a mediation path.")))
      if (identical(term, lhs))
        return(fail(paste0("Variable '", lhs, "' cannot predict itself.")))
      
      value <- suppressWarnings(as.numeric(coefs[[i]]))
      if (length(value) != 1 || is.na(value))
        return(fail(paste0("The path '", term, " -> ", lhs,
                           "' has no numeric coefficient. Every path needs a value, e.g. '",
                           lhs, " ~ .3*", term, "'.")))
      if (abs(value) < .001)
        return(fail(paste0("The coefficient for the path '", term, " -> ", lhs,
                           "' is too small (|value| must be at least .001).")))
      if (abs(value) > .99)
        return(fail(paste0("The coefficient for the path '", term, " -> ", lhs,
                           "' is too large (|value| must be at most .99).")))
      
      ladd(edges) <- list(from = term, to = lhs, value = value)
      used <- used + 1L

      ## a symbolic label (e.g. `a*.2*m1`) marks this edge as selectable for the
      ## sensitivity / minimum-detectable-effect analysis via `test:a`.
      sym <- if (i <= length(eq$coef_symbs)) eq$coef_symbs[i] else NA
      if (!is.na(sym)) labels[[sym]] <- c(lhs, term)   # c(to, from)
    }

    if (used == 0L)
      return(fail(paste0("The equation for '", lhs, "' has no predictors.")))
  }
  
  ## ---- build the variable set and the RAM path matrix A -------------------
  froms <- vapply(edges, `[[`, character(1), "from")
  tos   <- vapply(edges, `[[`, character(1), "to")
  vars  <- unique(c(froms, tos))                # all model variables
  
  A <- matrix(0, length(vars), length(vars), dimnames = list(vars, vars))
  for (e in edges) A[e$to, e$from] <- e$value   # A[i,j] = path FROM j TO i
  
  ## ---- the model must be recursive (acyclic) ------------------------------
  if (.mediation.has_cycle(A))
    return(fail(paste0("The model is not recursive: it contains a feedback loop. ",
                       "Mediation models must be acyclic (no variable feeds back into its own predictors).")))
  
  ## ---- and must contain at least one indirect (mediated) effect -----------
  paths <- .mediation.indirect_paths(A)
  if (length(paths) == 0)
    return(fail(paste0("The model has no indirect effect. A mediation model needs at least one ",
                       "path of the form X -> M -> Y (a variable that is both an outcome and a predictor).")))
  
  list(ok = TRUE, A = A, vars = vars, paths = paths, coef_labels = labels)
}


## ============================================================================
##  .mediation.build_S() : turn `cor(v1, v2) = r` commands into the residual
##  (disturbance) covariance matrix S used by .mediation.implied_cor(A, S).
##
##  `r` is the correlation between v1 and v2 and is placed directly into S
##  (S[v1, v2] = r), i.e. the lavaan `v1 ~~ r*v2` convention.
##
##  `cor_cmds` is the list of {args, value} parsed from the syntax (commands$cor).
##  Returns list(ok, S, cors) on success -- S is NULL when no correlations were
##  given -- or list(ok = FALSE, message) with a user-facing reason.
## ============================================================================

.mediation.build_S <- function(A, cor_cmds) {

  if (is.null(cor_cmds) || length(cor_cmds) == 0)
        return(list(ok = TRUE, S = NULL, cors = list()))

  vars <- rownames(A)
  S    <- matrix(0, nrow(A), ncol(A), dimnames = list(vars, vars))
  cors <- list()

  for (cc in cor_cmds) {
        if (length(cc$args) != 2)
              return(list(ok = FALSE,
                          message = paste0("cor() needs exactly two variables, e.g. cor(m1,m2)=.3 (got: ",
                                           paste(cc$args, collapse = ", "), ").")))
        v1 <- cc$args[1]; v2 <- cc$args[2]

        if (identical(v1, v2))
              return(list(ok = FALSE, message = paste0("cor(", v1, ",", v2,
                          "): a variable cannot be correlated with itself.")))
        miss <- setdiff(c(v1, v2), vars)
        if (length(miss))
              return(list(ok = FALSE, message = paste0("cor(): variable(s) '",
                          paste(miss, collapse = "', '"), "' are not in the model.")))

        r <- suppressWarnings(as.numeric(cc$value))
        if (length(r) != 1 || is.na(r))
              return(list(ok = FALSE, message = paste0("cor(", v1, ",", v2,
                          "): the correlation value '", cc$value, "' is not numeric.")))
        if (abs(r) >= 1)
              return(list(ok = FALSE, message = paste0("cor(", v1, ",", v2,
                          "): the correlation must be between -1 and 1.")))

        S[v1, v2] <- S[v2, v1] <- r
        cors[[length(cors) + 1L]] <- list(v1 = v1, v2 = v2, r = r)
  }

  list(ok = TRUE, S = S, cors = cors)
}


## ============================================================================
##  .mediation.diagram_layout() : mediation-style node layout for semPaths().
##  Each variable is placed in a column equal to its furthest position along any
##  indirect effect (X on the left, the final outcome on the right). For a single
##  chain the two endpoints are dropped below the mediators so they sit above the
##  X -> Y baseline (the classic triangle); when several variables share a column
##  they are stacked vertically.
##
##  `model` is a lavaanify() parameter table; `ie` is the list of indirect-effect
##  variable chains (obj$info$targets). Returns an n x 2 coordinate matrix in the
##  model's variable order, or "tree" when there are no indirect effects.
## ============================================================================

.mediation.diagram_layout <- function(model, ie) {

  if (is.null(ie) || length(ie) == 0) return("tree")

  lhs  <- model$lhs[model$op != ":="]
  xcoo <- suppressWarnings(
            sapply(unique(lhs), function(x)
                   max(unlist(lapply(ie, function(chain) which(chain == x))))))
  xcoo[!is.finite(xcoo)] <- 1

  q          <- cbind(seq_along(xcoo), order(xcoo))
  orig_order <- q[order(q[, 2]), 1]
  xcoo       <- xcoo[order(xcoo)]

  if (length(unique(xcoo)) == length(xcoo)) {
        ycoo <- rep(.80, length(xcoo))
        ycoo[xcoo == min(xcoo)] <- ycoo[xcoo == max(xcoo)] <- .2
  } else {
        ycoo <- unlist(lapply(unique(xcoo), function(x) {
              nvars <- length(xcoo[xcoo == x])
              seq_len(nvars) / (nvars + 1)
        }))
  }

  p <- cbind(x = xcoo, y = ycoo)
  p[orig_order, , drop = FALSE]
}

