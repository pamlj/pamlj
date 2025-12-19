## checkdata:       (required) this prepares all the info required to estimate the power parameters

.checkdata.pamlmixed <- function(obj) {


      if (is.something(obj$data)) 
        return()
      jinfo("Checking data for pamlmixed")

      ### first we take all information from the syntax
      syntaxobj<-try_hard(syntax_digest(obj$options$code))
      if (!isFALSE(syntaxobj$error)) obj$stop("Model formula not correct:" %+% syntaxobj$error)
      model<-syntaxobj$obj
      if (is.null(model$terms)) {
          obj$warning<-list(topic="issues",message="Please insert a linear mixed model in the syntax", head="info")
          obj$ok<-FALSE
          return()
      }
      # we need to be sure that intercepts are included in the model
      model<-fix_intercept(model,1)
      ## run some checks
      check_mixed_model(obj,model)
      if (!obj$ok) return()
      
      
      #### model syntax should be all digested at this point
      
      ## now we fill the info regarding the clusters
      syn_clusters<-model$clusters
      if (!is.something(syn_clusters)) 
        obj$stop("Please specify the random component is the  linear mixed model syntax")

      clusterpars<- obj$options$clusterpars
      names(clusterpars)<-sapply(obj$options$clusterpars, function(x) x$name)
      model$cluster_info<-lapply(syn_clusters, function(x) {
        acluster<-clusterpars[[x]]
        if (is.null(acluster)) stop("Cluster variable ",x," defined in syntax but not setup in the options")
        acluster$n<-as.numeric(acluster$n)
        if (is.na(acluster$n)) x$n<-2
        acluster$k<-as.numeric(acluster$k)
        if (is.na(acluster$k)) acluster$k<-5
        acluster
      })
      names(model$cluster_info)<-sapply(obj$options$clusterpars, function(x) x$name)
      model$expand<-model$clusters[1]
      ### Here we handle the variables
      
      ### var in cluster?
      test<-intersect(model$clusters,model$fixed$vars)
      if (is.something(test)) obj$stop("Variable " %+% paste(test,collapse=", ") %+% " cannot be a term and a cluster variable")

      ### actual variables that will be in the generated data
      model$variables<-c(model$fixed$vars,model$clusters)
      .vars_info<-obj$options$var_type
      names(.vars_info)<-sapply(.vars_info,function(x) x$name)
      issue<-list()
      model$variable_info<-lapply(model$varnames, function(x) {
        .var<-.vars_info[[x]]
        if (is.null(.var)) return(name=x,type="continuous",levels=2)
        if (.var$type=="continuous") .var$levels<-2
        if (.var$type=="categorical") .var$levels<-as.numeric(.var$levels)
        .var
      })
      names(model$variable_info)<-names(.vars_info)

      ### check number of coefficients
      test<-check_coefs_length(model,model$variable_info)
      if (!isTRUE(test)) obj$stop("Fixed effects: " %+% test)
      for (random in model$random)  {
        test<-check_coefs_length(random,model$variable_info)
        if (!isTRUE(test)) obj$stop("Random coefficients: " %+% test)
        
      }

      ## digest additional commands
      
      model<-extract_syntax_commands(obj,model)
      ## model should be completed now
    
      ## set the sub-aim  
      find <- obj$options$find
  
   
     ## check if input makes sense

      if (obj$aim=="n") {
        
        if (length(model$cluster)>1) {
               obj$warning<-list(topic="issues",message="Required N's are computed for the first cluster: " %+% names(clusters)[[1]], head="info")
               .clusters<-model$clusters[-1]
               test<-test_parameters(obj,.clusters, fun=function(x) (is.na(x$k) || x$k<5),head="Please insert the number of levels larger than 4 for cluster:")
               
        }
        
        if (find == "k") {
           test<-test_parameters(obj,model$cluster_info, fun=function(x) is.na(x$n),head="Please insert the number of cases for cluster:")
           test<-test_parameters(obj,model$cluster_info, fun=function(x) x$n<1,head="Minimum number of cases for a cluster is 1: Please correct `Cases` for cluster:")
           .clusters<-model$cluster_info[1]
           test<-test_parameters(obj,.clusters, fun=function(x) x$k>5,head="With aim=k (find # of clusters levels), the input cluster levels are used as starting point. Clusters:", fail = FALSE)
            }
     
      if (find == "n") { 
           test<-test_parameters(obj,model$cluster_info, fun=function(x) is.na(x$k),head="Please insert the number of levels for cluster:")
           test<-test_parameters(obj,model$cluster_info, fun=function(x) x$k<5,head="Minimum number of levels for a cluster is 5: Please correct `Levels` for cluster:")
           test<-test_parameters(obj,model$cluster_info, fun=function(x) x$n>0,head="With aim=n (find # of cases per cluster), the input number of cases are used as starting point. Clusters:", fail = FALSE)
       }
      }
      if (obj$aim=="power") {
           test<-test_parameters(obj,model$cluster_info, fun=function(x) is.na(x$n),head="Please insert the number of cases for cluster:")
           test<-test_parameters(obj,model$cluster_info, fun=function(x) x$n<1,head="Minimum number of cases for a cluster is 1: Please correct `Cases` for cluster:")
           test<-test_parameters(obj,model$cluster_info, fun=function(x) is.na(x$k),head="Please insert the number of levels for cluster:")
           test<-test_parameters(obj,model$cluster_info, fun=function(x) x$k<5,head="Minimum number of levels for a cluster is 5: Please correct `Levels` for cluster:")
      }
      
      #check cat vars are well-defined

      test<-test_parameters(obj,model$variable_info, fun=function(x) x$type=="categorical" && is.na(x$levels),head="Please insert the number of levels for categorical variable: ")
      
      ### check on vars ok
      
      model$sigma <- sqrt(obj$options$sigma2) 
      obj$data             <- data.frame(sig.level=obj$options$sig.level)
      obj$data$power       <- obj$options$power
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

      if (obj$options$algo=="mc" && obj$ok) {
        obj$info$parallel    <-  obj$options$parallel 
        obj$warning     <-  list(topic="initnotes",message="Monte Carlo method may take several minutes to estimate the results. Please be patient.", head="wait")
        if (obj$info$R<1000)
           obj$warning     <-  list(topic="issues",message="Simulations replications is set to " %+% obj$info$R %+% ". Please set a number greater than 1000 for more stable results.", head="info")
      
        obj$warning     <-  list(topic="initnotes",message="Monte Carlo methods may take several minutes to estimate the results. Please be patient.", head="wait")
        }    
      

      obj$info$sel_fun<-min
    
      ## interpret some commands
      if ("test" %in% names(model$commands)) {
        if (length(model$commands$test)>1) obj$stop("Only one effect can be tested in one run.")
        w<-which(model$coef_symbs==model$commands$test)
        if (length(w)==0) obj$stop("Test coefficient " %+% model$commands$test %+% " not found.")
        obj$info$sel_fun<-function(x) {
          x[[w-1]]
        }
      }
      
      obj$info$model  <- model
#      dat<-.make_data(obj,k=100)
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
           n<-obj$info$model$cluster_info[[1]]$n
           l <- obj$info$model$cluster_info[[1]]$k
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
           k<-obj$info$model$cluster_info[[1]]$k
           l <- obj$info$model$cluster_info[[1]]$n
           # if we have random slopes, start with n>2
           if (l < 4 && length(obj$info$model$random[[1]]$terms) > 1) l<-4
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
    
   
    .pow<-int_seek(f = f1,
                   target_power = obj$info$power,
                   n_start=l,
                   memory = 10,
                   tol=obj$options$tol/2, 
                   stability=obj$options$stability,
                   sel_fun = obj$info$sel_fun)  
     int<-.pow[[find]][[1]]
     out<-attr(.pow,"out")
     pwr<-attr(.pow,"power")
     rinfo("\nQuick search found " %+% find %+% "=" %+% .pow[[find]][[1]] %+% " with exit:" %+% out %+% "\n")
 
     if (!is.null(out) && out=="asymptote" && abs(obj$info$power-pwr)>.10 && obj$options$algo=="mc") {
       pow<-.slow_onerun(obj,n=.pow$n,k=.pow$k)
       msg<-"Preliminary power calculation indicates that the input model would not achieve the desired power. Power remains around " %+%
             round(pow$power,5) %+% " when " %+% what %+%" > " %+% pow[[find]] 
       if (is.something(attr(.pow,"sd")))
            msg <- msg %+% " with an average increase in power of " %+% round(attr(.pow,"sd"),5) %+% " per one unit increase in "  %+% what 
       obj$warning<-list(topic="issues",message=msg,head="warning")
       pow$sig.level<-obj$data$sig.level
       obj$data<-pow
       return(pow)
     }
     if (obj$options$algo=="mc") {
    
         pow<-int_seek(f = f2,
                       target_power = obj$info$power,
                       n_start=int,memory=5,
                       tol=obj$options$tol,
                       stability=obj$options$stability,
                       sel_fun = obj$info$sel_fun)  
         out<-attr(pow,"out")
         pwr<-attr(pow,"power")

         rinfo("\nMC search found " %+% find %+% "=" %+% pow[[find]][[1]] %+% " with exit:" %+% out %+% "\n")
         if (!is.null(out) && out=="asymptote" && abs(obj$info$power-pwr)> obj$options$tol) {
               msg<-"Power calculation indicates that the input model would not achieve the exact desired power. Power remains around " %+%
               round(pow$power,5) %+% " when " %+% what %+%" > " %+% pow[[find]] 
               if (attr(pow,"dir")==1)
                   dir <- "increase"
               else 
                   dir <- "decrease"
               msg <- msg %+%  " with an average " %+% dir %+% " in power of " %+% round(attr(pow,"sd"),5) %+% " per one unit " %+% dir %+% " in " %+% what 
              obj$warning<-list(topic="issues",message=msg,head="warning")
         }
     } else {
       pow <- .pow
     }
     pow$sig.level<-obj$data$sig.level
     if (length(obj$info$model$clusters)>1) {
       w<-which(names(obj$info$model$cluster_info)==obj$info$model$expand)
       target<-obj$info$model$cluster_info[[w]]
       info<-obj$info$model$cluster_info[-w]
       hm<-unlist(lapply(names(info), function(x) {
         cluster<-info[[x]]
         paste("cluster variable <b>",cluster$name, "</b> with n=",cluster$n," and k=",cluster$k)
         }))
       msg<-"Power parameters are computed for cluster variable <b>" %+% target$name %+% "</b>, setting " %+% paste(hm, collapse = ", ")
       obj$warning<-list(topic="powertab",message=msg)
     }
     pow<-as.data.frame(pow,stringsAsFactors = FALSE)
     pow$tested<-as.character("Est.")
     pow$tested[pow$power == obj$info$sel_fun(pow$power)] <- "Tested"
     obj$data<-pow

  } 
  if (obj$aim=="power") {
     if (obj$options$algo=="mc")
         .fun<-.slow_onerun
     else
         .fun<-.fast_onerun
     # we use the first cluster parameters
     n<-obj$info$model$random[[1]]$n
     k<-obj$info$model$random[[1]]$k
     pow<-.fun(obj,n=n,k=k)
     pow$sig.level<-obj$data$sig.level
     obj$data<-pow
  }
  return(pow)
}

.showdata.pamlmixed<-function(obj) {
  
  n<-obj$data$n[[1]]
  k<-obj$data$k[[1]]
  if (n>30) n<-30
  if (k>30) k<-30
  model<-pamlmixed_makemodel(obj,n,k)
  data<-model@frame
  if (nrow(data)>30) {
    data<-data[1:30,]
    warning("Only the first 30 observations are shown")
  }
  
  data$row<-1:nrow(data)
  return(data)
  
}


.showvars.pamlmixed<-function(obj) {

  n<-obj$data$n[1]
  k<-obj$data$k[1]
  mark(obj$data)
  if (n>1000) n<-1000
  if (k>30) k<-30
  model<-pamlmixed_makemodel(obj,n,k)
  data<-model@frame
  model<-obj$info$model
  results<-list()
  for (v in model$variable_info) {
    for (cl in model$clusters) {
      
     if (v$type=="continuous") {
        res<-mean(tapply(data[[v$name]],list(data[[cl]]),sd))
        res<-mean(tapply(data[[v$name]],list(data[[cl]]),sd))
        ev<-"  "
        if (res>.1) ev<-"within"
        ladd(results)<-list(var=v$name,cluster=cl,what="SD within",value=round(res,3),evalue=ev)
        res<-sd(tapply(data[[v$name]],list(data[[cl]]),mean))
        ev<-"  "
        if (res>.1) ev<-"between"

        ladd(results)<-list(var=v$name,cluster=cl,what="SD between",value=round(res,3),evalue=ev)
      }
      else {
           fun<-function(x) length(unique(x))
           res<-mean(tapply(data[[v$name]],list(data[[cl]]),fun))
           ev<-"mixed"
           if (v$levels==res) ev<-"within"
            if (round(res,0)==1) ev<-"between"
           ladd(results)<-list(var=v$name,cluster=cl,what="Levels within",value=round(res,3),evalue=ev)
       }
    }
  }

  ## freq of observations within cluster. Recall clusters can be cluster1:cluster2 or cluster
  for(term in names(model$random)) {
    vars<-strsplit(term,":",fixed=TRUE)[[1]]
    vars <- trimws(vars)
    vars <- vars[nzchar(vars)]
    idata<-data[,vars,drop=FALSE]
    for (var in vars)
         idata<-idata[idata[[var]]==levels(idata[[var]])[1],, drop=FALSE]
    value<-dim(idata)[1]
    ladd(results)<-list(var="  ",cluster=term,what="N observations",value=value,evalue="within")
  }
  return(results)

}



###### local functions

.make_data<-function(obj,n=NULL,k=NULL) {
  
  if (obj$options$stability=="l1")
    set.seed(obj$info$seed)
  
  model<-obj$info$model
  #### cluster data
  ks<-sapply(model$cluster_info, function(x) x$k)
  ## here we decide which cluster to expand
  w<-which(names(model$cluster_info)==model$expand)
  if (is.something(k)) ks[[w]]<-k
  levels<-lapply(ks,function(x) 1:x)
  cdata<-as.data.frame(expand.grid(rev(levels)))  
  names(cdata)<-rev(model$clusters)
  cdata<-cdata[,model$clusters,  drop = FALSE]
  ### within data
  ns<-sapply(model$cluster_info, function(x) x$n)
  if (is.something(n)) ns[[w]]<-n
  levels<-lapply(ns,function(x) 1:x)
  
  wdata<-as.data.frame(expand.grid(levels))
  names(wdata)<-paste0("inter_id",1:length(levels))
  ### put them together
  ldata<-list()
  ### we capture warning because it may complain about rownames
  try_hard({
    for (i in seq_len(nrow(cdata))) {
      one<-cbind(cdata[i,],wdata)
      ldata[[length(ldata)+1]]<-one
    }
  }) #try
  data<-do.call(rbind,ldata)
  names(data)<-c(model$clusters,names(wdata))

  ## first we deal with categorical within (default)
  ## the code is weired and ugly, but it handles a lot of different combinations of designs
  
  ## first, we select the categorcial variables
  cats<-lapply(model$variable_info,function(x) if (x$type=="categorical") return(x))
  cats<-cats[sapply(cats,function(x) !is.null(x))]
  if (length(cats)>0) {
  
    cats_within<-lapply(cats,function(x) if (is.null(x$between)) return(x))
    cats_within<-cats_within[sapply(cats_within,function(x) !is.null(x))]
    levels<-lapply(cats_within,function(x) 1:x$level)
  # since they are not between, we prepare their factorial combinations
    catdata<-as.data.frame(expand.grid(levels))
  
    nr<-nrow(catdata)
    
    datalist<-list()
  ## now, for each combination of clusters levels, we build the within factorial design
    for (i in seq_len(nrow(cdata))) {
      onelist<-lapply(cats_within, function(v) {
       ## we replicate their value within their cluster or (default) the combinations of clusters levels
         if (is.something(v$within)) cols<-v$within
         else cols<-model$clusters
         one<-merge(data,cdata[i,cols,drop=FALSE],by=cols)
         one<-cbind(one,.recycle_df(catdata[,v$name,drop=FALSE],nrow(one)))
         if (nrow(one)<nr) {
           obj$stop("N per cluster too small for the planned design")
         }
         one
        })
      # here we have one case (whatever it means)
        onedata<-.cbind_unique(onelist)
        ladd(datalist)<-onedata
    }
    if (length(datalist)>0)
        data<-as.data.frame(do.call(rbind,datalist))

  ### now we handle categorical between
    cats_between<-lapply(cats,function(x) if (!is.null(x$between)) return(x))
    cats_between<-cats_between[sapply(cats_between,function(x) !is.null(x))]
    for (x in cats_between) {
      cluster<-x$between
      cluster_info<-model$cluster_info[[cluster]]
      data[[x$name]]<-as.numeric(scale(rnorm(nrow(data))))
      data<-standardize_between(data,x$name,x$between)
      qs <- quantile(data[[x$name]], probs = seq(0, 1, length.out = x$levels + 1), na.rm = TRUE, type = 7)
      qs <- unique(qs)  
      data[[x$name]]<-cut(data[[x$name]], breaks = qs, include.lowest = TRUE)
      levels(data[[x$name]])<-letters[1:x$levels]
    }  
  ### at this point, we can factor the categorical variables and give them a contrast
    for (x in cats) {
      data[[x$name]]<-factor(data[[x$name]])
      contrasts(data[[x$name]])<-contr.sum(x$levels)
    }
  } ### end of factors
  
  for (x in model$cluster_info) {
    data[[x$name]]<-factor(data[[x$name]])
  } 
  
  ## now we deal with numeric variables 
  nums_vars<-lapply(model$variable_info,function(x) if (x$type=="continuous") return(x))
  nums_vars<-nums_vars[sapply(nums_vars,function(x) !is.null(x))]
  
  for (i in seq_along(nums_vars)) {
    x<-nums_vars[[i]]
    data[[x$name]]<-as.numeric(scale(rnorm(nrow(data))))
    if (is.something(x$within)) {
      data<-standardize_within(data,x$name,x$within)
    }
    if (is.something(x$between)) {
      data<-standardize_between(data,x$name,x$between)
    }
  }
    
  ## dependent 
  data[[model$lhs]]<-rnorm(nrow(data))
  ###
  names(data) <- trimws(make.names(names(data), unique = TRUE))
  attr(data,"n")<-ns[[w]]
  attr(data,"k")<-ks[[w]]
  data
  
}
### helper functions for making data
.recycle_df <- function(df, n) {
  k <- nrow(df)
  if (n %% k != 0L) {
    warning(sprintf("Partial recycling: target n = %d is not a multiple of source rows = %d",n, k),call. = FALSE  )
  }
  df[rep(seq_len(k), length.out = n), , drop = FALSE]
}

.cbind_unique <- function(dfs) {

  Reduce(function(df1, df2) {
    dup <- intersect(names(df1), names(df2))
    cbind(df1, df2[, setdiff(names(df2), dup), drop = FALSE])
  }, dfs)
}

### end of them

pamlmixed_makemodel <- function(obj,n=NULL,k=NULL) {

  data<-.make_data(obj,n=n,k=k)
  infomod<-obj$info$model
  
  varcor<-lapply(infomod$random,function(x) {
    coefs<-unlist(x$coefs)
    diag(x=coefs,nrow=length(coefs))
  })
  fixed<-unlist(infomod$coefs)
  
  modelobj<-try_hard({
             simr::makeLmer(formula=as.formula(infomod$formula),
                   fixef=fixed,
                   VarCorr=varcor,
                   sigma=infomod$sigma,
                   data=data
                   )
  })
  if (!isFALSE(modelobj$error)) {
    obj$stop("Model cannot be simulated. Please check your input syntax")
  }
  if (!isFALSE(modelobj$message)) {
    if (grep("rank",modelobj$message,fixed = TRUE)>0)
         obj$stop("Model cannot be simulated. Please check your input model")
  }
  
  model<-modelobj$obj
  attr(model,"n")<-attr(data,"n")
  attr(model,"k")<-attr(data,"k")
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
    tab$n=n
    if (is.null(n)) tab$n<-attr(model,"n")
    tab$k=k
    if (is.null(k)) tab$k<-attr(model,"k")
    tab$df=df
    tab$effect<-rownames(tab)
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
      test<-(!is.null(attr(asynlist$terms,"error")))
      if (test) {
        msg<-"Model syntax error in " %+% what %+% ": " %+% attr(asynlist$terms,"error")
        obj$stop(msg)
      }
      test<-(!is.null(attr(asynlist$coefs,"error")))
      if (test) {
      
        msg<-"Coefficients error in " %+% what %+% ": " %+% attr(asynlist$coefs,"error")
        obj$stop(msg)
      }
      
      if (attr(asynlist,"fix_intercept")) {
        obj$warning<-list(topic="issues",message=sprintf(msg1,what), head="info")
      }

      }
      
      fun(model,"fixed effects")
      for (n in names(model$random)) fun(model$random[[n]],"random coefficient across " %+% n)  
      
}

check_coefs_length<-function(model,variable_info) {

  for (i in seq_along(model$terms)) {
    tlist<-jmvcore::decomposeTerm(model$terms[[i]])
    l<-1
    for (t in tlist) {
      v<-variable_info[[t]]
      l<-l*(v$levels-1)
    }

    if(is.something(l) && !is.na(l) && l != length(model$coefs[[i]])) return("Number of coefficients for term " %+% model$terms[[i]] %+% " not correct")
    }
  return(TRUE)
}



extract_syntax_commands<-function(obj,model) {
  
  cmd<-model$commands
  if (any(c("bet","between") %in% names(cmd))) {
    for (x in cmd$bet) {
      str<-stringr::str_split(x,"\\|")[[1]]
      model$variable_info[[str[[1]]]]$between=str[[2]]
    }
  }
  if (any(c("wit","within") %in% names(cmd))) {
    for (x in cmd$wit) {
      str<-stringr::str_split(x,"\\|")[[1]]
      model$variable_info[[str[[1]]]]$within=str[[2]]
    }
  }
  for (v in names(model$variable_info)) {
    if (any(c("within","between") %in% names(model$variable_inf[[v]]))) {
      test1<-(model$variable_info[[v]]$within==model$variable_info[[v]]$between)
      test2<-(is.something(model$variable_info[[v]]$within) && is.something(model$variable_info[[v]]$between))
      if (any(test1,test2))
        obj$warning<-list(topic="issues",message="Variable " %+% v %+% " defined as both between and within clusters. It is modelled as between.", head="warning")
    }
  }
  if ("expand" %in% names(cmd)) {
     model$expand<-cmd$expand[[1]]
  }
    
    
  return(model)
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
  astep<-step
  cache<-new.env(parent = emptyenv())
  ### we keep track of the results to check if gets stuck
  cache$reslist<-1:memory
  cache$nlist<-c(lower)
  cache$max<-list(n=100000,v=1)
  cache$min<-list(n=0,v=0)
  iter<-0
  repeat{
    rinfo("INT_SEEK: trying n=",n," with boundaries [",cache$min$n,",",cache$max$n,"] ")
    iter<-iter+1
    res<-fun(n)
    oldn<-n
    pwr<-sel_fun(res$power)
    attr(res,"power")<-pwr
    stopifnot(is.finite(pwr))
    diff<-(pwr-target_power)
    adiff<-abs(diff)
    l<-length(cache$reslist)
    cache$reslist[[l+1]]<-adiff
    cache$reslist<-cache$reslist[-1]
    cache$nlist[[length(cache$nlist)+1]]<-n
    
    
    if (diff>0) dir<- -1 else dir<-1
    steps<-round(adiff*astep*dir)
    n<- n+steps
    s<-sd(unlist(cache$reslist))
    attr(res,"sd")<-s  
    attr(res,"dir")<-dir
    rinfo("obtained power =",round(pwr,5)," target=",target_power," next steps=",steps," sd=",s)
    if (stability=="l1") {
      if (n %in% cache$nlist) {
       n<-n+dir
      }
    }
    if (adiff < tol) {
      attr(res,"out")<-"found"
      return(res)
      
    }


    ## if power is always lower than target it may be stuck
    if (s<(tol/4)) {
      attr(res,"out")<-"asymptote"
      return(res)
    }
    
    if (iter>max_iter) {
      attr(res,"out")<-"maxiter"
      return(res)
    }
    if (steps==0) {
      attr(res,"out")<-"nosteps"
      return(res)
      
    }
    ## if we hit power=1 try the lowest possible n, otherwise it may get stuck
    if (pwr==1) n<-lower
    
    if (stability=="l1") p=1 else p=2
    #### now we want to be sure that we do not try N outside what was proved too large or too small
    if (pwr-p*tol > target_power) {
      if (pwr < cache$max$v) 
        cache$max<-list(n=oldn,v=pwr)
    }
    if (pwr+p*tol < target_power) {
      if (pwr > cache$min$v) 
        cache$min<-list(n=oldn,v=pwr)
    }
    # if max n and min n are the same or 1 unit apart, return
    if ((cache$max$n-cache$min$n) <= 1 ) {
      attr(res,"out")<-"closed"
      return(res)
    }
    if ((cache$max$n-cache$min$n) == 2 && n==(cache$min$n+1)) {
      attr(res,"out")<-"middle"
      return(res)
    }
    if (n <= cache$min$n)  n<-cache$min$n+1
    if (n >= cache$max$n)  n<-cache$max$n-1
    
    if (n<lower) {
      if (lower %in% cache$nlist) {
        steps<-round(steps/2)
        n <- lower + 1
      } else
        n <- lower
    }
    
    
  }
  
}

standardize_within <- function(data, x, cluster) {
  x      <- as.character(x)
  cluster <- as.character(cluster)
  
  # compute mean and sd per cluster
  means <- tapply(data[[x]], data[[cluster]], mean, na.rm = TRUE)
  sds   <- tapply(data[[x]], data[[cluster]], sd,   na.rm = TRUE)
  
  # build standardized vector
  z <- (data[[x]] - means[data[[cluster]]]) / sds[data[[cluster]]]
  
  # return data with new column
  data[[x]] <- z
  data
}


standardize_between <- function(data, x, cluster) {
  x       <- as.character(x)
  cluster <- as.character(cluster)
  
  # 1. cluster means of x
  cl_means <- tapply(data[[x]], data[[cluster]], mean, na.rm = TRUE)
  # cl_means is named by cluster levels
  
  # 2. standardize those means across clusters
  mu  <- mean(cl_means, na.rm = TRUE)
  sdx <- sd(cl_means,   na.rm = TRUE)
  
  z_cl <- (cl_means - mu) / sdx   # one z per cluster
  
  # 3. assign the same value to all rows in a cluster
  z_vec <- z_cl[ data[[cluster]] ]
  
  # 4. store in data
  new_name <- paste0(x, "_between_z")
  data[[x]] <- z_vec
  data
}


######### S3 output functions 

.effectsize_init.pamlmixed= function(obj) {
  
  try_hard({
    terms <- obj$info$model$terms
    coefs <-  obj$info$model$coefs
    terms[as.numeric(terms)==1]<-"(Intercept)"
    results<-list()
    for (i in seq_along(terms)){
      for (j in seq_along(coefs[[i]]))
        ladd(results)<-list(type="Fixed",term=terms[[i]],value=coefs[[i]][j],cluster=NA,es=NA,label=NA)
    } 
    
    
    random <- obj$info$model$random
    for (name in names(random)) {
      re<-random[[name]]
      for (i in seq_along(re$terms))
        for (j in seq_along(re$coefs[[i]])) {
          value<-re$coefs[[i]][j]
          ladd(results)<-list(type="Random",
                              term=re$terms[[i]],
                              value=value,
                              es=value/(value+obj$info$model$sigma^2),
                              cluster=name,
                              label="ICC")
        }
    }
    ladd(results)<-list(type="Variance",term=letter_sigma2,value=obj$info$model$sigma^2,label="Residual Variance",cluster=NA,es=NA)
  })
  return(results)
}

### S3 dispactchable table filling functions

.infotab_init.pamlmixed<- function(obj) {
  
      model<-obj$info$model
      coefs<-gsub(")","]",gsub("c(","[",paste(model$coefs, collapse=", "),fixed=T), fixed=T)
      
      tab<- list(list(info="Model",value=model$formula, specs=""),
             list(info="Fixed effects",value=model$rhs, specs=""),       
             list(info="Fixed coefs",value=coefs, specs=" "),       
            list(info="Clusters:",value=" ",specs=" ")
            )
            for (cluster in model$clusters) ladd(tab)<-list(info="Clusters:",value=cluster)
            ladd(tab)<-list(info="Variables:",value=" ",specs=" ")
            for (var in model$variable_info) ladd(tab)<-list(info="Variables:",value=var$name,specs=var$type  )
            tab
}

