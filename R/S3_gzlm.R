## checkdata:       (required) this prepares all the info required to estimate the power parameters

.checkdata.gzlm <- function(obj) {

    
      obj$data<-data.frame(es=abs(obj$options$es))
      obj$data$n           <- obj$options$n
      obj$data$sig.level   <- obj$options$sig.level
      obj$data$power       <- obj$options$power
      obj$data$df_effect   <- obj$options$df_effect
      obj$data[[obj$aim]]  <- NULL
      obj$info$letter      <- letter_eta2
      obj$info$esmax       <- .99999
      obj$info$esmin       <-  1e-05
      obj$info$nmin        <-  4
      obj$info$nmax        <-  1e+04
      obj$info$p0          <- obj$options$p0
      obj$info$nlevels      <- obj$options$nlevels
      obj$info$model_type  <- obj$options$model_type
      
      if (obj$info$model_type=="multinomial" && obj$data$df_effect < (obj$info$nlevels-1))
           obj$stop("In multinomial model, effect degrees of freedom cannot be less than the number of levels
                    in the dependent variables minus one. In fact, the df of the effect should be a multiple of 
                    the number of levels minus 1.")

       if (obj$info$model_type=="multinomial" && obj$data$df_effect %% (obj$info$nlevels-1))
           obj$stop("In multinomial model, effect degrees of freedom of the effect should be a multiple of 
                    the number of levels minus 1.")

}

## powervector:     (required) pass the data, with adjutment, to the lowerlevel power function if necessary,
##                  and produces the power parameters, for any row in data                   

.powervector.gzlm <- function(obj,data) {

              
                 aim<-required_param(data)
                 k  <-  obj$info$nlevels
                 p0 <- obj$info$p0
                 p1 <- (1-p0)/(k-1)
                 D0 <- -2*(p0*log(p0)+(k-1)*p1*log(p1))
                 
                 if (aim=="es")
                        data$lambda<-NULL
                 else 
                        data$lambda<-sqrt(data$es*D0)
                   
                 .names<-names(data)
              
                 results<-lapply(1:nrow(data),function(i) {
                        one<-as.list(data[i,.names])
                        pwr::pwr.chisq.test(N=one$n,w=one$lambda,df=one$df_effect,power=one$power, sig.level=one$sig.level)
                 })
                 results<-as.data.frame(do.call(rbind,results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 results[["n"]]<-ceiling(results[["N"]])
                 results[["N"]]<-NULL
                 results[["test"]]<-results[["w"]]^2
                 results[["df_effect"]]<-results[["df"]]
                 results[["es"]]<-results[["w"]]^2/D0
                 results[["note"]]<-NULL
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

