## checkdata:       (required) this prepares all the info required to estimate the power parameters

.checkdata.pamlsem <- function(obj) {

      jinfo("Checking data for pamlsem")

      syntax<-obj$options$code
      syntax<-stringr::str_remove_all(syntax," ")

      if (stringr::str_length(syntax)==0) {
        obj$ok<-FALSE
        obj$warning<-list(topic="issues",message="Please input the SEM model in the syntax box", head="info")
        return()
      }
      spsyntax   <-  strsplit(syntax,"\\n")[[1]]      
      ### population model
      popModel   <-  gsub("\\*\\s*[A-Za-z]\\s*\\*", "\\*", spsyntax)
      keep       <-  grep("==|:=",popModel,invert=T)
      popModel   <-  popModel[keep]
      str_popModel   <-  paste(popModel,collapse="\n")
  
      ### H models

      h0         <-  gsub("(?<![A-Za-z])\\b\\d+(\\.\\d+)?\\*\\b", "",spsyntax,  perl = TRUE)
      h0         <-  gsub('\\.\\*|\\.',"",h0)
      keep       <-  grep("==",h0,invert=T)
      h1         <-  h0[keep]
      if (length(h0)==length(h1)) obj$stop("Please specify at least one parameter to be tested with `parm==0`")
      
      str_h0         <-  paste(h0,collapse="\n")
      str_h1         <-  paste(h1,collapse="\n")

      if (obj$options$standardized) {
        ### here comes the magic standardization :-)
        modelobj<-try_hard(lavaan::sem(popModel))
        if (!isFALSE(modelobj$error)) {
          obj$stop(modelobj$error)
        }
        model          <-  modelobj$obj
        dsigma         <-  diag(lavaan::inspect(model,"implied")$cov)
        exvar          <-  rep(2,length(dsigma))-dsigma
        exvar          <-  exvar[exvar!=1]
        varstr         <-  paste(lapply(names(exvar),function(x) paste0(x,"~~",exvar[x],"*",x) ), collapse="\n")
        str_popModel   <-  paste(paste(str_popModel,"\n"),varstr,collapse="\n")
      }

     
      obj$data             <- data.frame(n=obj$options$n)
      obj$data$sig.level   <- obj$options$sig.level
      obj$data$power       <- obj$options$power
      if (obj$options$alternative=="one-tailed") obj$data$sig.level<-obj$data$sig.level*2 
      obj$data$estimator   <- obj$options$estimator
      obj$data$modelPop    <- str_popModel
      obj$data$modelH0     <- str_h0
      obj$data$modelH1     <- str_h1
      obj$data[[obj$aim]]  <- NULL
      obj$info$letter      <- "RMSEA"
      obj$info$esmax       <-  10
      obj$info$esmin       <-  1e-06
      obj$info$nmin        <-  10
      obj$info$nochecks    <-  "es"
      obj$info$method      <- obj$options$method
      obj$plots$data       <- obj$data
      obj$data$simulatedPower <- (obj$options$method == "mc") 
      
      if (obj$data$simulatedPower) {
        ### we need sigma for this
        modelobj<-try_hard(lavaan::sem(popModel))
        if (!isFALSE(modelobj$error)) {
          obj$stop(modelobj$error)
        }
        model           <-  modelobj$obj
        obj$info$sigma  <-  lavaan::inspect(model,"implied")$cov
        obj$warning     <-  list(topic="initnotes",message="Monte Carlo method may take several minutes to estimate the results. Please be patient.", head="info")
        obj$data$R      <- obj$options$mcR
        obj$data$parallel <- obj$options$parallel
        if (obj$options$set_seed)
                obj$data$seed   <- obj$options$seed
      }
      jinfo("Checking data for pamlsem done")
}



## powervector:     (required) pass the data, with adjutment, to the lowerlevel power functions 

.powervector.pamlsem <- function(obj,data) {

  
                 data$alpha <- data$sig.level
                 
                 if (any(is.null(data[["n"]]))) 
                    data$type="a-priori"
                 else { 
                    data$type="post-hoc"
                 }
                 ## the actual function that estimates the power parameters
                 FUN=pamlj.semanalytic
                 if (any(data$simulatedPower)) {
                  
                 ## the actual function that estimates the power parameters
                    FUN=pamlj.semmc
                 }
                 results<-lapply(1:nrow(data),function(i) {
                     
                     one      <- as.list(data[i,])
                     if (one$simulatedPower) one$sigma<-obj$info$sigma
                     tryobj<-try_hard(do.call(FUN,one))
                     out<-tryobj$obj
                     if (!isFALSE(tryobj$error)) obj$stop(tryobj$error)
                     out
                    })
                  results<-as.data.frame(do.call(rbind,results))   
                  
                  for (name in names(results)) results[[name]]<-unlist(results[[name]])
                  odata<-data[, !names(data) %in% names(results)]
                  results<-cbind(odata,results)
                  results$simulatedPower<-FALSE
                  return(results)
  
}


## powertab_init:   (not required) this function produces or format the main table, powertab, before running

.powertab_init.pamlsem <- function(obj) {

          if (!obj$ok) return()
          tab<-list(list(effect="H1 vs H0"))
          if (obj$data$simulatedPower) 
             obj$warning<-list(topic="powertab",message="Power parameters are computed for Score test for the input constraints. ")
          return(tab)
          
}



## powertab:        (not required) this function produces or format the main table, powertab, after running
## No need

## powerbyes:       this function produces or format the powerbyes table , after the estimation is done
## powerbyen:       this function produces or format the powerbyen table , after the estimation is done
## effectsize_init: (not required) some sub.module requires additional effect size to be computed. This function inits the table


## effectsize_run:  (not required) some sub.module requires additional effect size to be computed. This function fills the table



## extrainfo:       (better if is there) this function add some extra info to be given to the user when the option "explain" is selected

.extrainfo.pamlsem <- function(obj) {
 
   if (!obj$option("explain")) return()  
  
}

### find parameters need some specs for mediation

.find_min_n.pamlsem <- function(obj,data) {
  
  jinfo("find min n for pamlsem")
  sp<-semPower::semPower.aPriori(effect = data$es, effect.measure = "RMSEA", 
                       alpha = data$sig.level,df=data$df,power=.10)
  return(sp$requiredN)
}

.find_max_n.pamlsem <- function(obj,data) {
  jinfo("find max n for pamlsem")
  sp<-semPower::semPower.aPriori(effect = data$es, effect.measure = "RMSEA", 
                       alpha = data$sig.level,df=data$df,power=.99)
  return(sp$requiredN)
}

.find_max_es.pamlsem <- function(obj,data) {
  return(.99)
}

.find_min_es.pamlsem <- function(obj,data) {
  return(.0001)
}
