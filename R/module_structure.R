# Initer handles all the init procedures for setting up the tables
# Runner handles the estimation of the parameters and the filling of  tables.
# For each sub-module, Runner gets two classes, the "model_type" and the "mode"
# Thus, we need to prepare S3 functions to handles the different sub-modules. 
# 
# Here we define the S3 methods that are rquired for each sub-module
#
# methods are:
## checkdata:       (required) this prepares all the info required to estimate the power parameters
## rundata :       (not required) process input before running powervector in run phase
## powervector:     (required) pass the data, with adjutment, to the lowerlevel power functions 
## powertab_init:   (not required) this function produces or format the main table, powertab, before running
## powertab:        (not required) this function produces or format the main table, powertab, after running
## powerbyes:       this function produces or format the powerbyes table , after the estimation is done
## powerbyen:       this function produces or format the powerbyen table , after the estimation is done
## effectsize_init: (not required) some sub.module requires additional effect size to be computed. This function inits the table
## effectsize_run:  (not required) some sub.module requires additional effect size to be computed. This function fills the table
## extrainfo:       (better if is there) this function add some extra info to be given to the user when the option "explain" is selected
## find_*_es, find_*_max:  find parameters is usually ok for any sub-module, but we may need to specialize it


## checkdata: this prepares all the info required to estimate the power parameters

checkdata <- function(obj, ...) UseMethod(".checkdata")


## rundata :       (not required) process input before running powervector in run phase

rundata <- function(obj, ...) UseMethod(".rundata")

.rundata.default <- function(obj) return()

## powervector must accept a runner object and a data.frame. It must return a data.frame with nrow() equal to the input data.frame
## they are used across all table and plots to estimate parameters, so the input data.frame is not necessarely the 
## orginal input data of the user.
## Differently to other software, these functions cannot fail. They should return a value (possibly Inf or 0) in any case.
## For this to happen, input data must be checked for plausibility in checkdata()
## Functions  are always called but return different information
## depending to the analysis being carried out

powervector <- function(obj, ...) UseMethod(".powervector")


### this function produces or format the main table, powertab

powertab_init <- function(obj, ...) UseMethod(".powertab_init")

.powertab_init.default <- function(obj) {
  
          if (!obj$ok) return()
  
          tab <-  obj$data
          if (!is.null(obj$data)) 
                 attr(tab,"titles")<-list(es=obj$info$letter)  
          return(tab)
          
}

### this function produces or format the main table , powertab, after the estimation is done

powertab <- function(obj, ...) UseMethod(".powertab")

.powertab.default <- function(obj) return(obj$data)


### this function produces or format the powerbyes table , after the estimation is done


powerbyes <- function(x, ...) UseMethod(".powerbyes")

.powerbyes.default <- function(obj) {

           
            power = c(.5, .8, .95)
            data<-obj$data
            data$power<-NULL
            suppressWarnings(dd<-as.data.frame(cbind(power,data)))
            dd$es<-NULL
            res<-powervector(obj,dd)
            probs_es<-format(res$es,digits=3)
            check<-which(is.na(res$es))
           
            if (length(check)>0) warning("Some effect size cannot be computed given the input parameters.")

            esList <-list(list(es=paste('0 <', obj$info$letter, greek_vector["leq"],probs_es[1])),
                          list(es=paste(probs_es[1],'<', obj$info$letter, greek_vector["leq"],probs_es[2])),
                          list(es=paste(probs_es[2],'<', obj$info$letter, greek_vector["leq"],probs_es[3])),
                          list(es=paste(obj$info$letter,">" ,probs_es[3]))
            )
            return(esList)
            
}


### this function produces or format the powerbyn table , after the estimation is done

powerbyn <- function(x, ...) UseMethod(".powerbyn")

.powerbyn.default <- function(obj) {

            power = c(.5, .8, .95)
            data<-obj$data
            dd<-do.call(rbind,lapply(power, function(x) {
                                 data$power<-x
                                 return(data)}))
            dd$n<-NULL    
            results<-powervector(obj,dd)
            results$n<-round(results$n,digits=0)

            esList <-list(
                          list(n=results$n[1]),
                          list(n=results$n[1] %+% "-" %+% results$n[2]),
                          list(n=results$n[2] %+% "-" %+% results$n[3]),
                          list(n=results$n[3])
                      )
            return(esList)
            
}

### this function add some extra info to be given to the user when the option "explain" is selected

extrainfo <- function(obj, ...) UseMethod(".extrainfo")

.extrainfo.default <-function(obj) return()


### some sub.module requires additional effect size to be computed. They go into the effectsize table
### _init prepares the table, _run fills in it 

effectsize_init <- function(obj, ...) UseMethod(".effectsize_init")

effectsize_run <- function(obj, ...) UseMethod(".effectsize_run")

.effectsize_init.default <- function(obj) return()

.effectsize_run.default <- function(obj) return()



### find parameters is usually ok for any sub.module, but we may need to specialize it

find_min_n <- function(obj, ...) UseMethod(".find_min_n")

.find_min_n.default <- function(obj,data) {
  
  data$n <- NULL
  data$power<-.10
  res<-try_hard(powervector(obj,data))
  if (isFALSE(res$error)) {
    n<-ceiling(res$obj$n)
  }
  else
    n<-obj$info$nmin
   return(n)
}



find_max_n <- function(obj, ...) UseMethod(".find_max_n")

.find_max_n.default <- function(obj,data) {
  
  data$es<-ifelse(data$es/3 > obj$info$esmin, data$es/3, obj$info$esmin)
  data$power=.99
  data$n <- NULL
  res<-try_hard(powervector(obj,data))
  if (isFALSE(res$error)) 
    n<-ceiling(res$obj$n)
  else
    n<-obj$info$nmax

return(n)
}


find_max_es <- function(obj, ...) UseMethod(".find_max_es")

.find_max_es.default <- function(obj,data) {


  data$es <- NULL
  res<-try_hard(powervector(obj,data))
  if (isFALSE(res$error)) 
    es<-res$obj$es
  else
    es<-obj$info$esmax

return(es)
}

find_min_es <- function(obj, ...) UseMethod(".find_min_es")

.find_min_es.default <- function(obj,data) {

  if ("es" %in% obj$info$nochecks) return(obj$info$esmin)
    
  data$es <- NULL
  if (is.null(data$power)) data$power=.99
  data$n  <- obj$info$nmax
  res<-try_hard(powervector(obj,data))
  if (isFALSE(res$error)) 
    es<-res$obj$es
  else
    es<-obj$info$esmin
return(es)
}


required_param<-function(data) {
  
  whichnull<-setdiff(c("n","es","sig.level","power"), names(data))  
  if (length(whichnull)>1 || length(whichnull)==0) {
         stop("PAMLj: only one parameters should be NULL: here we have ", paste(whichnull,collapse=", "))
  }
  whichnull
}
