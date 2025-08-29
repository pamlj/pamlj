## checkdata:       (required) this prepares all the info required to estimate the power parameters

.checkdata.pamlsem <- function(obj) {

      if (is.something(obj$data)) 
        return()
      jinfo("Checking data for pamlsem")

      syntax<-obj$options$code
      syntax<-stringr::str_remove_all(syntax," ")

      if (stringr::str_length(syntax)==0) {
        obj$ok<-FALSE
        obj$warning<-list(topic="issues",message="Please input the SEM model in the syntax box", head="info")
        return()
      }
      spsyntax   <-  strsplit(syntax,"\\R", perl=TRUE)[[1]]      
#      mark(syntax,spsyntax)
      ### population model
      popModel   <-  gsub("\\*\\s*[A-Za-z]\\s*\\*", "\\*", spsyntax)
      keep       <-  grep("==|:=",popModel,invert=T)
      popModel   <-  popModel[keep]
      str_popModel   <-  paste(popModel,collapse="\n")
  
      ### H models

      h0         <-  gsub("(?<![A-Za-z])\\b\\d+(\\.\\d+)?\\*\\b", "",spsyntax,  perl = TRUE)
      h0         <-  gsub('\\.\\*|\\.',"",h0)
      consts     <-  grep("==",h0)
      obj$info$constraints<-stringr::str_split(h0[consts],"\n")
      keep       <-  grep("==",h0,invert=T)
      h1         <-  h0[keep]
      if (length(h0)==length(h1)) obj$stop("Please specify at least one parameter to be tested with `parm==0`")

      str_h0         <-  paste(h0,collapse="\n")
      str_h1         <-  paste(h1,collapse="\n")
      
      modelobj<-try_hard(lavaan::sem(popModel))
        if (!isFALSE(modelobj$error)) {
          obj$stop(modelobj$error)
      }
      model          <-  modelobj$obj
      obj$info$lvnames<-lavaan::lavNames(model,type="lv")
      obj$info$ovnames<-lavaan::lavNames(model,type="ov")

  
      
      if (obj$options$standardized) {
        ### here comes the magic standardization :-)
        
        dsigma         <-  diag(lavaan::inspect(model,"implied")$cov)
        exvar          <-  rep(2,length(dsigma))-dsigma
        exvar          <-  exvar[exvar!=1]
        varstr         <-  paste(lapply(names(exvar),function(x) paste0(x,"~~",exvar[x],"*",x) ), collapse="\n")
        str_popModel   <-  paste(paste(str_popModel,"\n"),varstr,collapse="\n")
        lsigma         <-  diag(lavaan::inspect(model,"cov.lv"))
      
        if (length(lsigma)>0) {
            exvar          <-  rep(2,length(lsigma))-lsigma
            exvar          <-  exvar[exvar!=1]
            varstr         <-  paste(lapply(names(exvar),function(x) paste0(x,"~~",exvar[x],"*",x) ), collapse="\n")
            str_popModel   <-  paste(str_popModel,varstr, sep="\n")
        }

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
      obj$data$test        <-  obj$options$mc_test 
      
      if (obj$data$simulatedPower) {
        ### we need sigma for this
        obj$info$sigma  <-  lavaan::inspect(model,"implied")$cov
        obj$warning     <-  list(topic="initnotes",message="Monte Carlo method may take several minutes to estimate the results. Please be patient.", head="wait")
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
          if (obj$data$simulatedPower)  {
            if (obj$data$test == "score")
                     obj$warning<-list(topic="powertab",message="Power parameters are computed with Monte Carlo method for Score test for the input constraints. ")
            else
                     obj$warning<-list(topic="powertab",message="Power parameters are computed with Monte Carlo method for LRT test comparing constrained vs unconstrained model. ")
          }
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
  
   conds<-stringr::str_split(obj$info$constraints,"==")
   add<-""
   plural<-""
   verb <- " is "
   if (length(conds)>1) {
       add<-"any of "
       plural<-"s "
   }
    text<-"Testing " %+% add %+% "the following condition" %+% plural %+% ": <br> <ul>"
     for (x in conds) {
     add<-""
     verb <- " is "
     plural<-" "
     cond<-x[-length(x)]
     if (length(cond)>1) {
              add<-"all "
              verb<-" are "
              plural<-"s "              
     }
     astring<-"<b>" %+% paste(cond,collapse=", ") %+% "</b>"
     text<-text %+% "<li>" %+% add %+%  " coefficient" %+% plural %+% astring  %+% verb %+% " statistically significant  </li>  "
   }
    text <- text %+% "</ul>"
    if (obj$aim=="n")
        text <- text %+% "<p> To meet the previous requirements, one requires a minimum sample size of N=" %+% round(obj$data$n) %+% "</p>"
    else
        text <- text %+% "<p> Meeting the previous requirements, given the input sample size, yields a power of " %+% format(obj$data$power,digits=3)  %+% "</p>"
      
    obj$warning<-list(topic="extrainfo",message=text,head="info")  
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


