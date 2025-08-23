## checkdata:       (required) this prepares all the info required to estimate the power parameters

.checkdata.pamlmixed <- function(obj) {

      if (is.something(obj$data)) 
        return()
      jinfo("Checking data for pamlmixed")

      
      clustersopt<-obj$options$clusterpars
      find <- obj$options$find
    
      syntax<-obj$options$code
      spsyntax   <-  stringr::str_split_1(syntax,"\\n")  
      spsyntax   <- spsyntax[grep("~",spsyntax)]

      syntax<-stringr::str_remove_all(syntax," ")

      clusters<-lapply(clustersopt, function(x) list(k=as.numeric(x$k),n=as.numeric(x$n)))
      names(clusters)<-unlist(sapply(clustersopt, function(x) x$name))                                              
      variables<-lapply(obj$options$var_type, function(x) list(name=x$name,type=x$type,levels=as.numeric(x$levels)))
      names(variables)<-unlist(lapply(variables,function(x) x$name))

      ## check input 
      ## any model
      if (stringr::str_length(syntax)==0) {
        obj$ok<-FALSE
        obj$warning<-list(topic="issues",message="Please input the linear mixed model in the syntax box (see info for details)", head="info")
        return()
      }

      
      ## amy cluster?
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
        if (find == "k") {
           test<-test_parameters(obj,clusters, fun=function(x) is.na(x$n),head="Please insert the number of cases for cluster:")
           test<-test_parameters(obj,clusters, fun=function(x) x$n<2,head="Minimum number of cases for a cluster is 2: Please correct `Cases` for cluster:")
            }
     
      if (find == "n") { 
           test<-test_parameters(obj,clusters, fun=function(x) is.na(x$k),head="Please insert the number of levels for cluster:")
           test<-test_parameters(obj,clusters, fun=function(x) x$k<5,head="Minimum number of levels for a cluster is 5: Please correct `Levels` for cluster:")

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
     
      modelobj    <-  try_hard(decompose_mixed_formula(spsyntax))
      if (!isFALSE(modelobj$error)) obj$stop("Model formula not correct:" %+% modelobj$error)
      model <- modelobj$obj
      ## check the model syntax
      fixed<-model$fixed
      check_mixed_model(obj,fixed,"Fixed")

      re<-model$re
      lapply(re, function(x) check_mixed_model(obj,x,"Random"))
      model$clusters<-unique(names(clusters))
      .terms<-model$fixed$terms[-1]
      ### assess variables structure
      model$variables<- lapply(variables, function(x) {
         clusters<-unlist(lapply(model$clusters, function(z) {
            if (length(grep(x$name,re[[z]]))>0)
                  return(z)
           }))
         if (length(clusters)>0)
               x$cluster<-clusters
         return(x)
      })

      names(model$variables)<-.terms
      ### assess cluster size
      model$re<-lapply(obj$options$clusterpars, function(x) {
         re<-model$re[[x$name]]
         x$n<-as.numeric(x$n)
         if (is.na(x$n)) x$n<-2
         x$k<-as.numeric(x$k)
         if (is.na(x$k)) x$k<-10
         x$coefs<-re$coefs
         x$terms<-re$terms
         x$name<-NULL
         x
         })
      names(model$re)<-model$clusters
    
      re<-paste(lapply(names(model$re), function(x) {
        .re<-model$re[[x]]
        .terms <- paste(.re$terms,collapse=" + ")
         paste("(",.terms,"|",x,")")
         }), collapse=" + ")
      model$formula<-paste(model$fixed$lhs,"~",paste(model$fixed$terms,collapse=" + "),"+",re,collapse=" + ")
       

      obj$data             <- data.frame(sig.level=obj$options$sig.level)
      obj$data$power       <- obj$options$power
      obj$info$model       <- model
      obj$info$letter      <- "Coef"
      obj$info$esmax       <-  10
      obj$info$esmin       <-  1e-06
      obj$info$nmin        <-  10
      obj$info$nochecks    <-  "es"
      obj$plots$data       <- obj$data
      obj$info$R           <-  obj$options$mcR 
      obj$info$set_seed    <-  obj$options$set_seed 
      obj$info$seed        <-  obj$options$seed 
      obj$info$parallel    <-  obj$options$parallel 
      obj$warning     <-  list(topic="initnotes",message="Monte Carlo method may take several minutes to estimate the results. Please be patient.", head="wait")
      
      jinfo("Checking data for pamlmixed done")
}



## powervector must accept a runner object and a data.frame. It must return a data.frame with nrow() equal to the input data.frame
## they are used across all table and plots to estimate parameters, so the input data.frame is not necessarily the 
## orginal input data of the user.
## Differently to other software, these functions cannot fail. They should return a value (possibly Inf or 0) in any case.
## For this to happen, input data must be checked for plausibility in checkdata()
## Functions  are always called but return different information
## depending to the analysis being carried out

.powervector.pamlmixed <- function(obj,data) {
  
  find<-obj$options$find
  
  if (obj$aim=="n") {
    if (find == "k") {
           n<-obj$info$model$re[[1]]$n
           l <- 5
           f<-function(int) {
             pamlmixed_onerun(obj,n,int)
           }
    }
    if (find == "n") {
           k<-obj$info$model$re[[1]]$k
           l <- 2
           f<-function(int) {
             pamlmixed_onerun(obj,int,k)
           }
    }
    .pow<-int_seek(f,"power",obj$info$power,lower=l,upper=2000)  
    pow<-.pow$f
    obj$data<-pow
  } 
  if (obj$aim=="power") {
    
     pow<-pamlmixed_onerun(obj,obj$info$model$re[[1]]$n,obj$info$model$re[[1]]$k)
     obj$data<-pow
  }
  
  return(pow)
  
           
  
}


###### local functions


pamlmixed_makemodel <- function(obj,n,k) {
  
  infomod<-obj$info$model
  fixed<-as.list(infomod$fixed$coefs)
  data<-lme4::mkDataTemplate(as.formula(infomod$formula),nGrps=k,nPerGrp=n,rfunc=rnorm)
  for (i in seq_along(infomod$variables)) {
    x<-infomod$variables[[i]]
    if (x$type=="categorical") {
      data[[x$name]]<-cut(data[[x$name]],breaks=x$levels,labels=1:x$levels)
      xcoefs<-fixed[[i]]
      if (x$levels>2) xcoefs<-c(fixed[[i]],rep(0,x$levels-2)) 
      fixed[[i]]<-xcoefs
    }
  }
  
  varcor<-lapply(infomod$re,function(x) {
    diag(x=x$coefs,nrow=length(x$coefs))
  })
  fixed<-unlist(fixed)
  modelobj<-try_hard({
    simr::makeLmer(as.formula(infomod$formula),
                   data=data,
                   fixef=fixed,
                   VarCorr=varcor
                   ,sigma=1)
  })

  if (!isFALSE(modelobj$error)) {
    obj$stop(modelobj$error)
  }
  
  return(modelobj$obj)
  
} 


pamlmixed_onerun <- function(obj,n,k) {
  
  
  model<-pamlmixed_makemodel(obj,n,k)
  
  if (obj$info$parallel) {
    if (Sys.info()['sysname'] == "Windows") 
      plan<-future::multisession
    else                 
      plan<-future::multicore
  }
  RNGkind("L'Ecuyer-CMRG")
  future::plan(plan)
  lmodel<-lmerTest::as_lmerModLmerTest(model)
  onefun<-function() {
    y<-stats::simulate(lmodel)$sim_1
    fit<-simr::doFit(y,lmodel)
    fit<-lmerTest::as_lmerModLmerTest(fit)
    anov<-stats::anova(fit)
    anov$name<-rownames(anov)
    anov 
  }
  ## sims<-rbind,lapply(1:3,function(x) onefun())
  R<-obj$info$R
  if (obj$info$parallel)
    sims<-foreach::foreach(i = 1:R, .options.future = list(seed = TRUE)) %dofuture%  onefun()
  else
    sims<-rbind(lapply(1:R,function(x) onefun()))
                
  res<-as.data.frame(do.call(rbind,sims))
  res$power<- (res[,6] < obj$info$sig.level)
  pow<-lapply(c("NumDF", "DenDF","F value","Pr(>F)","power"), function(name) tapply(res[[name]],res$name,mean, na.rm=T))
  pow<-as.data.frame(do.call(cbind,pow))
  names(pow)<-c("df","df_error","F","p","power")
  pow$n<-n
  pow$k<-k
  pow$effect<-rownames(pow)
  return(pow)
                
 }




decompose_formula<-function(astring) {
  
          results<-list()
          warns<-list(int=FALSE,ivalue=FALSE,coefs=FALSE)
         .fixform<-gsub("\\d*\\.?\\d*\\s*\\*", "", astring)
         .s<-stringr::str_split_1(.fixform,"~")
         if (length(.s)>1) {
             results$lhs<-trimws(.s[[1]])
             .rhs<-.s[[2]]
         } else
             .rhs<-.s
         .coefs  <- as.numeric(unlist(regmatches(astring, gregexpr("\\d*\\.?\\d+(?=\\s*\\*)", astring, perl=TRUE))))
         .terms   <- unlist(trimws(stringr::str_split_1(string=.rhs,pattern="\\+")))
          int<-stringr::str_split_1(astring,"\\+")[[1]]
          if (length(grep("1",int))==0) {
              warns$int<-TRUE
             .rhs <- paste("1 +",.rhs)
             .coefs<-c(0,.coefs)
             .terms   <- c("1",.terms)
           }
          if (length(grep("1",int))>0 && length(grep("\\*",int))==0 ) {
                  .coefs<-c(0,.coefs)
                   warns$ivalue<-TRUE
          }
          results$rhs<-gsub(" ","",.rhs)

          if (length(.coefs) != length(.terms))
                warns$coefs=TRUE
          results$coefs   <-  .coefs
          results$terms   <- .terms
          attr(results,"warnings")<-warns
          return(results)
}


decompose_mixed_formula<-function(astring) {

          results<-list()
          warns<-list()
         ### handle fixed
          .fixed <- deparse(lme4::nobars(as.formula(astring)))
          .re    <- lme4::findbars(as.formula(astring))
          if (is.null(.fixed)) stop("No fixed terms in the model, please refine the input model")
          if (is.null(.re)) stop("No random coefficients in the model, please refine the input")
          results$fixed<-decompose_formula(.fixed)
          re<-lapply(.re, function(x) {
                 .input<-deparse(x[[2]])
                 .terms<-decompose_formula(.input)
                 .terms
          })
          names(re)<-unlist(lapply(.re,function(x) as.character(x[[3]])))
          results$re<-re
          return(results)
}

check_mixed_model<- function(obj,mmterms, type, cluster=NULL) {
  
      text<-ifelse(type=="Random","Variance for the random", "Fixed") 
      .target<- ifelse(is.null(cluster),".","for cluster" %+% cluster %+% ".")
      .attr<-attr(mmterms,"warnings")
      if (.attr$int) {
        msg<- type %+% " intercept was not explicitly declared" %+% .target %+%
              " It has been added to the model." %+%
              " It's expected value has been set to 0." %+%
              " To remove this warning, please use <b><i>y~value*1+...</i></b> syntax," %+%
              " where <b><i> value </i></b> is the expected value of the intercept, including zero values" %+%
              " <br> See <b>Info</b> for details. <br> <br>"
        obj$warning<-list(topic="issues",message=msg, head="info")
      }
      if (.attr$ivalue) {
        msg<- text %+% " intercept expected value was not explicitly declared" %+%
               .target %+%
               " It has been set to zero. To remove this warning, please use `y~value*1+...` syntax," %+%
               " where `value` is the expected value of the intercept, including zero values.  <br><br>"
        obj$warning<-list(topic="issues",message=msg, head="info")
      }
      if (.attr$coefs) {
        msg<- " Not all expected values have been declared for the " %+% tolower(type) %+% " effects." %+%
              " Please input all the expected value using the syntax <b><i> ... value*x+..</i></b>, " %+%
              " where <b><i>value</i></b> is the expected effect size (expected value), including zero values. <br> <br>"
        obj$warning<-list(topic="issues",message=msg, head="info")
      }

  
  
}

### a sort of uniroot for integers and lower checks

# Integer-only proportional seeker
# Assumes f is non-decreasing in n (important for termination).
int_seek <- function(f, value, t, lower = -Inf, upper = Inf,
                     s = 10,                 # proportionality factor (bigger => larger jumps)
                     max_iter = 100L,         # safety cap
                     tol = 0.01) {               # optional tolerance on the target
  # normalize inputs
  if (!is.finite(lower)) lower <- -.Machine$integer.max
  if (!is.finite(upper)) upper <-  .Machine$integer.max
  upper <- as.integer(upper)
  n<-lower  
  steps <- 0L
  inc<- 0
  repeat {
    res <- f(n)
    y<-min(res[[value]])
    mark(paste("int_seek trying",n,"with result",y,"inc:",inc))
    
    if (y > t - tol) {
      return(list(f=res, n = n, value = y, steps = steps, hit_upper = FALSE))
    }
    if (n >= upper) {
      return(list(f=res, n = n, value = y, steps = steps, hit_upper = TRUE))
    }
    gap <- t - y                 # in [0,1]
    inc <- as.integer(ceiling(s * gap))  # proportional to gap

    if (inc < 1L) inc <- 1L
    if (n + inc > upper) inc <- upper - n
    n <- n + inc
    steps <- steps + 1L
    if (steps >= max_iter) {
      return(list(f=res, n = n, value = f(n), steps = steps, hit_upper = (n >= upper)))
    }
  }
}
