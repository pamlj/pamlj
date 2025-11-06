## checkdata:       (required) this prepares all the info required to estimate the power parameters

.checkdata.pamlmixed <- function(obj) {

      if (is.something(obj$data)) 
        return()
      jinfo("Checking data for pamlmixed")

      
      clustersopt<-obj$options$clusterpars
      find <- obj$options$find
    
      suppressWarnings({
      clusters<-lapply(clustersopt, function(x) list(k=as.numeric(x$k),n=as.numeric(x$n)))
      names(clusters)<-unlist(sapply(clustersopt, function(x) x$name))                                              
      variables<-lapply(obj$options$var_type, function(x) list(name=x$name,type=x$type,levels=as.numeric(x$levels)))
      names(variables)<-unlist(lapply(variables,function(x) x$name))
      })
      ## check input 
      ## any model
      if (stringr::str_length(obj$options$code)==0) {
        obj$ok<-FALSE
        obj$warning<-list(topic="issues",message="Please input the linear mixed model syntax", head="info")
        return()
      }

      
      ## any cluster?
      if (!is.something(clustersopt)) {
        msg<-"<p>Please specify at the cluster variable(s) parameters</p>."
        obj$warning<-list(topic="issues",message=msg,head="info")
        obj$ok<- FALSE
        return()
      }
      
      if (!is.something(clusters)) {
        msg<-"<p>Please specify at least one clustering variable in the model with the syntax: </p> <p>+(1|cluster)</p>."
        obj$warning<-list(topic="issues",message=msg,head="info")
        obj$ok<- FALSE
        return()
      }
      ### var in cluster?
      test<-intersect(names(clusters),names(variables))
      if (is.something(test)) obj$stop("Variable " %+% paste(test,collapse=", ") %+% " cannot be a term and a cluster variable")
      

      if (obj$aim=="n") {
        
        if (length(clusters)>1) {
               obj$warning<-list(topic="issues",message="Required N's are computed for the first cluster: " %+% names(clusters)[[1]], head="info")
               .clusters<-clusters[-1]
               test<-test_parameters(obj,.clusters, fun=function(x) (is.na(x$k) || x$k<5),head="Please insert the number of levels larger than 4 for cluster:")
               
        }
        
        if (find == "k") {
           test<-test_parameters(obj,clusters, fun=function(x) is.na(x$n),head="Please insert the number of cases for cluster:")
           test<-test_parameters(obj,clusters, fun=function(x) x$n<2,head="Minimum number of cases for a cluster is 2: Please correct `Cases` for cluster:")
           .clusters<-clusters[1]
           test<-test_parameters(obj,.clusters, fun=function(x) x$k>5,head="With aim=k (find # of clusters levels), the input cluster levels are used as starting point. Clusters:", fail = FALSE)
            }
     
      if (find == "n") { 
           test<-test_parameters(obj,clusters, fun=function(x) is.na(x$k),head="Please insert the number of levels for cluster:")
           test<-test_parameters(obj,clusters, fun=function(x) x$k<5,head="Minimum number of levels for a cluster is 5: Please correct `Levels` for cluster:")
           test<-test_parameters(obj,clusters, fun=function(x) x$n>2,head="With aim=n (find # of cases per cluster), the input number of cases are used as starting point. Clusters:", fail = FALSE)
       }
      }
      if (obj$aim=="power") {
           test<-test_parameters(obj,clusters, fun=function(x) is.na(x$n),head="Please insert the number of cases for cluster:")
           test<-test_parameters(obj,clusters, fun=function(x) x$n<2,head="Minimum number of cases for a cluster is 2: Please correct `Cases` for cluster:")
           test<-test_parameters(obj,clusters, fun=function(x) is.na(x$k),head="Please insert the number of levels for cluster:")
           test<-test_parameters(obj,clusters, fun=function(x) x$k<5,head="Minimum number of levels for a cluster is 5: Please correct `Levels` for cluster:")
      }
      

      lapply(variables, function(x) {
          if (x$type == "categorical") {
           if (is.na(x$levels) || x$level<2)
              obj$stop("Please insert number of levels larger than 2 for variable " %+% x$name )
           if (x$level>2)
              obj$warning<-list(topic="issues",message="Input coefficient for variable " %+% x$name %+% " is applied to the largest mean difference", head="info")
          }
      })
           
  
      #### now work on the syntax
      syntax<-obj$options$code
      ### we get the line with a model, if more than one, we get the first
      model_line<-get_regression_lines(syntax)
      if (length(model_line)==0) obj$stop("Please insert a linear mixed model in the syntax")
      modelobj<-try_hard(decompose_mixed_formula(model_line, fix_intercept=TRUE, intercept_coef=0))
      if (!isFALSE(modelobj$error)) obj$stop("Model formula not correct:" %+% modelobj$error)
      model <- modelobj$obj
     
      check_mixed_model(obj,model)
      if (!obj$ok) return()
      fixed<-model$fixed
      model$clusters<-unique(names(clusters))

      ### assess variables structure
      model$variables<- lapply(variables, function(x) {
         clusters<-unlist(lapply(model$clusters, function(z) {
            if (length(grep(x$name,model$re[[z]]))>0)
                  return(z)
           }))
         if (length(clusters)>0)
               x$cluster<-clusters
         return(x)
      })

      names(model$variables)<-model$fixed$terms[-1]
      ### assess cluster size
      model$re<-lapply(obj$options$clusterpars, function(x) {
         re<-model$re[[x$name]]
         re$n<-as.numeric(x$n)
         if (is.na(re$n)) re$n<-2
         re$k<-as.numeric(x$k)
         if (is.na(re$k)) re$k<-5
         re
         })
      names(model$re)<-model$clusters
      re<-paste(lapply(names(model$re), function(x) {
        .re<-model$re[[x]]
         paste("(",.re$rhs,"|",x,")")
         }), collapse=" + ")
      model$formula<-paste(model$fixed$lhs,"~",model$fixed$rhs,"+",re,collapse=" + ")
      model$sigma <- sqrt(obj$options$sigma2) 

      obj$data             <- data.frame(sig.level=obj$options$sig.level)
      obj$data$power       <- obj$options$power
      obj$info$model       <- model
      obj$info$letter      <- "Coef"
      obj$info$esmax       <-  10
      obj$info$esmin       <-  1e-06
      obj$info$nmin        <-  10
      obj$info$nochecks    <-  "es"
      obj$plots$data       <-  obj$data
      obj$info$R           <-  obj$options$mcR 
      # the sim algorithm works much better with a seed. If the user passes a seed, we use it
      # otherwise we generate a random seed that is used for one estimation run only
      if(obj$options$set_seed) obj$info$seed <-  obj$options$seed else obj$info$seed <- as.integer(sample.int(.Machine$integer.max, 1L))

      obj$info$parallel    <-  obj$options$parallel 
      obj$warning     <-  list(topic="initnotes",message="Monte Carlo method may take several minutes to estimate the results. Please be patient.", head="wait")
      if (obj$info$R<1000)
         obj$warning     <-  list(topic="issues",message="Simulations replications is set to " %+% obj$info$R %+% ". Please set a number greater than 1000 for more stable results.", head="info")
      
      obj$warning     <-  list(topic="initnotes",message="Monte Carlo method may take several minutes to estimate the results. Please be patient.", head="wait")
      
      
      jinfo("Checking data for pamlmixed done")
}



## powervector must accept a runner object and a data.frame. It must return a data.frame with nrow() equal to the input data.frame
## they are used across all tables and plots to estimate parameters, so the input data.frame is not necessarily the 
## orginal input data of the user.
## Differently to other software, these functions cannot fail. They should return a value (possibly Inf or 0) in any case.
## For this to happen, input data must be checked for plausibility in checkdata()
## Functions  are always called but return different information
## depending to the analysis being carried out


### here we take a quite strange path. The issue is that simulations are very slow, so slow that
### regular users cannot phantom. So, we use a few tricks to speed up the process. Bear with me, please.
### it's a bit crooked, but is still much less code and time than  chatgpt suggestions (that were all unbearable)

.powervector.pamlmixed <- function(obj,data) {
  
  find<-obj$options$find
  

  if (obj$aim=="n") {
    if (find == "k") {
           what<-"number of clusters levels"  
           n<-obj$info$model$re[[1]]$n
           l <- obj$info$model$re[[1]]$k
           rinfo("Finding number of clusters\n")
           ## we want f() to search for n
           f1<-function(int) {
             .fast_onerun(obj,n=NULL,k=int)
           }
           f2<-function(int) {
             .slow_onerun(obj,n=NULL,k=int)
           }
           
    }
    if (find == "n") {
           what<-"number of cases"  
           k<-obj$info$model$re[[1]]$k
           l <- obj$info$model$re[[1]]$n
           # if we have random slopes, start with n>2
           if (l < 4 && length(obj$info$model$re[[1]]$terms) > 1) l<-4
           rinfo("Finding number of cases within clusters\n")
           ## we want f() to search for k
           f1<-function(int) {
             .fast_onerun(obj,int,k=NULL)
           }
           f2<-function(int) {
             .slow_onerun(obj,int,k=NULL)
           }
           
    }
    
    ## first, we search for a reasonable solution with anova-on-model based .fast_onerun() function, embeed in f1()
    ## then, we use .slow_onerun(), embedded in f2(), to find the more adequate solution
    
    .pow<-int_seek(f = f1,target_power = obj$info$power,n_start=l,memory = 10,tol=obj$options$tol/2, stability=obj$options$stability)  
     int<-.pow[[find]][[1]]
     out<-attr(.pow,"out")
     rinfo("\nQuick search found " %+% find %+% "=" %+% .pow[[find]][[1]] %+% " with exit:" %+% out %+% "\n")
  
     if (!is.null(out) && out=="asymptote" && (obj$info$power-.pow$power)>.10) {
       pow<-.slow_onerun(obj,n=.pow$n,k=.pow$k)
       msg<-"Preliminary power calculation indicates that the input model would not achieve the desired power. Power remains around " %+%
             round(pow$power,5) %+% " when " %+% what %+%" > " %+% pow[[find]]
       obj$warning<-list(topic="issues",message=msg,head="warning")
       pow$sig.level<-obj$data$sig.level
       obj$data<-pow
       return(pow)
     }
    
     pow<-int_seek(f = f2,target_power = obj$info$power,n_start=int,memory=3,tol=obj$options$tol,stability=obj$options$stability)  
     out<-attr(pow,"out")
     rinfo("\nMC search found " %+% find %+% "=" %+% pow[[find]][[1]] %+% " with exit:" %+% out %+% "\n")
     if (!is.null(out) && out=="asymptote") {
       msg<-"Power calculation indicates that the input model would not achieve the exact desired power. Power remains around " %+%
         round(pow$power,5) %+% " when " %+% what %+%" > " %+% pow[[find]] 
       msg <- msg %+% ". Larger starting points for " %+% what %+% " may find a more accurate solution."
       obj$warning<-list(topic="issues",message=msg,head="warning")
     }
     pow$sig.level<-obj$data$sig.level
     obj$data<-pow
  } 
  if (obj$aim=="power") {
     # we use the first cluster parameters
     n<-obj$info$model$re[[1]]$n
     k<-obj$info$model$re[[1]]$k
     pow<-.slow_onerun(obj,n=n,k=k)
     pow$sig.level<-obj$data$sig.level
     obj$data<-pow
     
  }
  
  return(pow)
  
           
  
}


###### local functions


pamlmixed_makemodel <- function(obj,n=NULL,k=NULL) {

  if (obj$options$stability=="l1")
      set.seed(obj$info$seed)
  
  
  infomod<-obj$info$model
  fixed<-as.list(infomod$fixed$coefs)
#  data<-lme4::mkDataTemplate(as.formula(infomod$formula),nGrps=k,nPerGrp=n,rfunc=rnorm)
  #mark(infomod)
  #### cluster data
  ks<-sapply(infomod$re, function(x) x$k)
  if (is.something(k)) ks[[1]]<-k
  levels<-lapply(ks,function(x) 1:x)
  cdata<-as.data.frame(expand.grid(levels))
  names(cdata)<-infomod$clusters
  ### within data
  ns<-sapply(infomod$re, function(x) x$n)
  if (is.something(n)) ns[[1]]<-n
  levels<-lapply(ns,function(x) 1:x)
  wdata<-as.data.frame(expand.grid(levels))
  names(wdata)<-paste0("inter_id",1:length(levels))
  ### put them together
  ldata<-list()
  for (i in seq_len(nrow(cdata))) {
    one<-cbind(cdata[i,],wdata)
    ldata[[length(ldata)+1]]<-one
  }

  data<-do.call(rbind,ldata)
  
  names(data)<-c(infomod$clusters,names(wdata))
  
  r<-nrow(data)
  for (i in seq_along(infomod$variables)) {
    x<-infomod$variables[[i]]
    data[[x$name]]<-rnorm(r)
    if (x$type=="categorical") {
      rep<-round(r/x$levels)
      data[[x$name]]<-factor(rep(1:x$levels,rep+100)[1:r])
      contrasts(data[[x$name]])<-contr.poly(x$levels)
      xcoefs<-fixed[[i]]
      if (x$levels>2) xcoefs<-c(fixed[[i]],rep(0,x$levels-2)) 
      fixed[[i]]<-xcoefs
    }
  }

  ## dependent 
  data[[infomod$fixed$lhs]]<-rnorm(r)
  ###
  varcor<-lapply(infomod$re,function(x) {
    diag(x=x$coefs,nrow=length(x$coefs))
  })
  fixed<-unlist(fixed)

  names(data) <- trimws(make.names(names(data), unique = TRUE))
  form <- as.formula(infomod$formula)
  modelobj<-try_hard({
    simr::makeLmer(formula=form,
                   fixef=fixed,
                   VarCorr=varcor,
                   sigma=infomod$sigma,
                   data=data
                   )
  })
  if (!isFALSE(modelobj$error)) {
    obj$stop(modelobj$error)
  }
  model<-modelobj$obj
  attr(model,"n")<-ns[[1]]
  attr(model,"k")<-ks[[1]]
  return(model)
  
} 


.fast_onerun <- function(obj, n = NULL, k = NULL) {
  
    model  <- pamlmixed_makemodel(obj, n, k)    
    tab <- car::Anova(model, type = 3)
    tab <- tab[-1,]
    df  <- tab[, "Df"]
    chisq <-tab[, "Chisq"]
    crit  <- stats::qchisq(1 - obj$data$sig.level, df)
    power <- 1 - stats::pchisq(crit, df = df, ncp = chisq)  
    tab$F<-chisq/df
    tab$power<-power
    tab$n=attr(model,"n")
    tab$k=attr(model,"k")
    tab

}

.sim_fun <- function(model, tol_sing = 1e-4, include_warnings = TRUE) {
  # 1) simulate new response
  y <- stats::simulate(model)$sim_1
  
  # 2) fit while capturing warnings
  warn_buf <- character(0)
  fit <- withCallingHandlers(
    expr = simr::doFit(y, model),
    warning = function(w) {
      warn_buf <<- c(warn_buf, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  # 3) cast to lmerTest
  fit <- lmerTest::as_lmerModLmerTest(fit)
  
  # 4) derive convergence + singularity
  #    (be defensive against optimizer variants)
  optinfo <- fit@optinfo
  # messages from lme4 and optimizer
  msgs <- c(
    tryCatch(optinfo$conv$lme4$messages, error = function(e) character(0)),
    tryCatch(optinfo$conv$opt$messages,  error = function(e) character(0)),
    warn_buf
  )
  msgs <- msgs[!is.na(msgs)]
  
  # optimizer code (0 means success for bobyqa/nlminb etc.)
  opt <- tryCatch(optinfo$conv$opt, error = function(e) NULL)
  code <- tryCatch(
    if (is.null(opt)) NA_integer_
    else if (is.list(opt) && !is.null(opt$code)) as.integer(opt$code)
    else if (is.numeric(opt)) as.integer(opt)
    else NA_integer_,
    error = function(e) NA_integer_
  )
  
  # heuristic: converged if no messages and (code is 0 or NA but no "failed"/"converge" warnings)
  converged <- TRUE
  if (length(msgs)) {
    # any "failed to converge" / "converge" in messages → not converged
    if (any(grepl("fail|converg", msgs, ignore.case = TRUE))) converged <- FALSE
  }
  if (!is.na(code)) {
    converged <- converged && (code == 0L)
  }
  
  singular <- lme4::isSingular(fit, tol = tol_sing)
  
  # 5) anova table + annotations
  anov <- stats::anova(fit)
  anov$name <- rownames(anov)
  anov$converged <- rep(converged, nrow(anov))
  anov$singular  <- rep(singular,  nrow(anov))
  if (isTRUE(include_warnings)) {
    anov$warnings <- rep(if (length(msgs)) paste(unique(msgs), collapse = " | ") else "", nrow(anov))
  }
  
  anov
}

# .sim_fun<-function(model) {
#   y<-stats::simulate(model)$sim_1
#   fit<-simr::doFit(y,model)
#   fit<-lmerTest::as_lmerModLmerTest(fit)
#   anov<-anova(fit)
#   anov$name<-rownames(anov)
#   anov
# }

.slow_onerun <- function(obj,n=NULL,k=NULL) {
  

  master_seed<-obj$info$seed
  
  model<-pamlmixed_makemodel(obj,n,k)
  R <- obj$info$R
  base::RNGkind("L'Ecuyer-CMRG")
    
  if (isTRUE(obj$info$parallel)) {
      plan <- if (Sys.info()[["sysname"]] == "Windows") future::multisession else future::multicore
      future::plan(plan)
      sims <- foreach::foreach(
        i = seq_len(R),
        .options.future = list(seed = master_seed)  # common random numbers CRN: deterministic substream per i
      ) %dofuture% {
        .sim_fun(model)
      }
    } else {
      # serial, still CRN: use per-rep substreams derived from the master seed
      set.seed(master_seed, kind = "L'Ecuyer-CMRG")
      sims <- lapply(seq_len(R), function(i) {
        set.seed(master_seed + i, kind = "L'Ecuyer-CMRG")
        .sim_fun(model)
      })
    }

  res<-as.data.frame(do.call(rbind,sims))
  res$power<- (res[,6] < obj$data$sig.level)
  pow<-lapply(c("NumDF", "DenDF","F value","Pr(>F)","power","converged","singular"), function(name) tapply(res[[name]],res$name,mean, na.rm=T))
  pow<-as.data.frame(do.call(cbind,pow))
  names(pow)<-c("df","df_error","F","p","power","converged","singular")
  pow$effect<-rownames(pow)
  pow$n<-attr(model,"n")
  pow$k<-attr(model,"k")
  return(pow)
                
}


check_mixed_model<- function(obj,model) {
  
  
  msg1 <- "<br>Intercept was not explicitly declared for %s " %+%
          "It has been added to the model." %+%
          "It's expected value has been set to 0 " %+%
          "To remove this warning, please use <b><i>y~value*1+...</i></b> syntax " %+%
          "where <b><i> value </i></b> is the expected value of the intercept, including zero values."
  msg2 <- "<br>Intercept expected value was not explicitly declared for %s " %+%
          "It has been set to zero. To remove this warning, please use <i><b>y~value*1+...</i></b>` syntax, " %+%
          "where `value` is the expected value of the intercept, including zero values. "
  msg3 <- "<br>Not all expected values have been declared for the %s." %+%
          "Please input all the expected value using the syntax <b><i> ..value*x+..</i></b>, " %+% 
          "where <b><i>value</i></b> is the expected effect size (expected value), including zero values."
  
  
      fun <- function(asynlist,what) {
      ### check the uniqueness
      test<-attr(asynlist$terms,"unique")
      if (!test) {
        msg<-"The model syntax contains not unique variables" %+% paste(obj$terms,collapse = ",")
        obj$stop(msg)
      }
      test<-attr(asynlist$terms,"error")

      if (test) {
        msg<-"Model syntax error in fixed effects"
        obj$stop(msg)
      }
      intadded<-FALSE

      if (attr(asynlist$terms,"intadded")) {
        intadded <- TRUE
        obj$warning<-list(topic="issues",message=sprintf(msg1,what), head="info")
      }
      if (!intadded && attr(asynlist$coefs,"intadded")) {
        msg<- 
          obj$warning<-list(topic="issues",message=sprintf(msg2,what), head="info")
      }
      if (any(is.na(asynlist$coefs))) {
        obj$stop(sprintf(msg3,what))
      }

      }
      
      fun(model$fixed,"fixed effects")
      for (n in names(model$re)) fun(model$re[[n]],"random coefficient across " %+% n)  
      
      
}

# Integer search on f(n)$power (assuming monotone increasing with m), starting at n_start.
# f(n) must return a list with a numeric scalar `power`.

## The function works with any function f(n), but it is tailored for function using long simulations
## First, it evaluated f(n) and compare the f($)$power with target_power
## if abs(f($)$power - target_power ) < tol we found a solution and return it
## the solution is not found n is changed based on steps=adiff*step*dir
## adiff is the absolute difference between observed and target power
## step is a multiplier passed as an argument (100 seems to work)
## dir is the direction to go increase (dir=+1)  or descrease (dir=-1) n
## However, when dealing with power simulations different things may go wrong, and a solution is not guaranteed, so
## the search has different checks and possible outcomes.
## 1) step is decreases every interation to reducing the bouncing up and down of n
## 2) the boundaries of n are recorded so if the steps algorithm yield outside the boundaries it does not run a new simulation
##    but simply changes the n. Boundaries are defined as the n that has yielded the minimum power larger than target and
##    the n that yielded the maximal power lower than target. 
##  3) because some mixed model would never give a required power for not incredibly huge n, the algorithm
##     keeps track of the standard deviation of the last simulations (how many is decided my "memory" argument).
##     if the sd goes below tolerance, it return the last result (it meant the algorithm got stuck in a loop or an asynthode).
##  4) if the algorithm suggests no change in n it returns the results (this should no happen, so it's a extra check)
##  5) if nothing works, max_iter makes sure that it does not go on forever

int_seek<-function(fun,sel_fun=min,n_start,target_power=.90,tol=.01,step=100,lower=2, max_iter=100, memory=5, stability="l1") {
  
  n<-n_start
  steps<-0
  cache<-new.env(parent = emptyenv())
  ### we keep track of the results to check if gets stuck
  cache$reslist<-list()
  cache$max<-list(n=100000,v=1)
  cache$min<-list(n=0,v=0)
  iter<-0
  repeat{
    rinfo("INT_SEEK: trying n=",n," with boundaries [",cache$min$n,",",cache$max$n,"] ")
    iter<-iter+1
    step<-step-1
    res<-fun(n)
    oldn<-n
    pwr<-sel_fun(res$power)
    stopifnot(is.finite(pwr))
    diff<-(pwr-target_power)
    adiff<-abs(diff)
    cache$reslist[[as.character(n)]]<-adiff
    if (adiff < tol) {
      attr(res,"out")<-"found"
      return(res)
      
    }
    rinfo("obtained power =",round(pwr,5)," target=",target_power)
    
    if (diff>0) dir<- -1 else dir<-1
    steps<-round(adiff*step*dir)
    n<- n+steps
    if (stability=="l1") {
      if (as.character(n) %in% names(cache$reslist)) {
       n<-n+dir
      }
    }
 
    if (n<lower) {
      attr(res,"out")<-"tolow"
      return(res)
    }
    ## if power is always lower than target it may be stuck
    if (length(cache$reslist) > memory && diff < 0) {
      s<-sd(unlist(cache$reslist))
      if (s<tol/2) {
        attr(res,"out")<-"asymptote"
        return(res)
      }
      cache$reslist[[1]]<-NULL
    }
    if (iter>max_iter) {
      attr(res,"out")<-"maxiter"
      return(res)
    }
    if (steps==0) {
      attr(res,"out")<-"nosteps"
      return(res)
      
    }
    #### now we want to be sure that we do not try N outside what was proved too large or too small
    if (pwr-2*tol > target_power) {
      if (pwr < cache$max$v) 
        cache$max<-list(n=oldn,v=pwr)
    }
    if (pwr+2*tol < target_power) {
      if (pwr > cache$min$v) 
        cache$min<-list(n=oldn,v=pwr)
    }
    if (n < cache$min$n)  n<-cache$min$n+1
    if (n > cache$max$n)  n<-cache$max$n-1
    
    # if max n and min n are the same or 1 unit apart, return
    if ((cache$max$n-cache$min$n) < 1.1 ) {
      attr(res,"out")<-"nosteps"
      return(res)
      }
  }
  
}






