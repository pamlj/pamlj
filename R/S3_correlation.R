## checkdata:       (required) this prepares all the info required to estimate the power parameters

.checkdata.correlation <- function(obj) {

     if (obj$options$es < 0)
                    obj$warning<-list(topic="issues",
                                     message="Negative correlations have the same power parameters of positive correlations. The absolute value is considered here.",
                                     head="info")
                                     
      obj$data<-data.frame(es=abs(obj$options$es))
      obj$data$n           <- obj$options$n
      obj$data$sig.level   <- obj$options$sig.level
      obj$data$power       <- obj$options$power
      obj$data$alternative <- obj$options$alternative
      obj$data[[obj$aim]]  <- NULL
      obj$info$letter      <- greek_vector["rho"]
      obj$info$esmax       <- .99999
      obj$info$esmin       <-  1e-07
      obj$info$nmin        <-  4


}

## powervector:     (required) pass the data, with adjutment, to the lowerlevel power functions 

.powervector.correlation <- function(obj,data) {

                 aim<-required_param(data)
                 
                 names(data)[names(data)=="es"]<-"r"
                .names <- intersect(names(data),rlang::fn_fmls_names(pwr::pwr.r.test))
                 data$alternative<-ifelse(data$alternative=="two.sided","two.sided","greater")


                results<-lapply(1:nrow(data),function(i) {
                     one      <-as.list(data[i,.names])
                     tryobj<-try_hard(do.call(pwr::pwr.r.test,one), silent=TRUE)
                     out<-tryobj$obj
                     if (!isFALSE(tryobj$error)) {
                       
                     switch(aim,
                            n = {
                                   n<-obj$info$nmin
                                   out<-list(n=n,r=one$r,sig.level=one$sig.level,power=one$power,alternative=one$alternative,method="nmin")
                               },
                            power={ 
                                   out<-list(n=one$n,r=one$r,sig.level=one$sig.level,power=NA,alternative=one$alternative,method="error")
                                  },
                            es={ 
                                   out<-list(n=one$n,r=NA,sig.level=one$sig.level,power=one$power,alternative=one$alternative,method="error")
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
                 results$es <- results$r
                 results$r  <- NULL
                 return(results)
  
}

## powertab:    (not required) this function produces or format the main table, powertab, after running
## NO NEED

## powerbyes:       this function produces or format the powerbyes table , after the estimation is done
## NO NEED

## powerbyen:       this function produces or format the powerbyes table , after the estimation is done
## NO NEED

## effectsize_init: (not required) some sub.module requires additional effect size to be computed. This function inits the table
## effectsize_run:  (not required) some sub.module requires additional effect size to be computed. This function fills the table
## NO NEED

## extrainfo:       (better if is there) this function add some extra info to be given to the user when the option "explain" is selected

## to be implemented