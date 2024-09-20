## S3 functions for proportion

## checkdata:       (required) this prepares all the info required to estimate the power parameters


.checkdata.propind <- function(obj) {

      obj$info$nmin         <- 4
  
      obj$data<-data.frame(power=obj$options$power)
      obj$data$n1          <- obj$options$propind_n
      obj$data$n_ratio     <- obj$options$propind_nratio
      obj$data$n2          <- ceiling(obj$data$n1 * obj$data$n_ratio)
      obj$data$n           <- obj$data$n1 + obj$data$n2
      obj$data$p1          <- obj$options$propind_p1
      obj$data$p2          <- obj$options$propind_p2
   
      
      .common_proportions(obj)


}

.checkdata.propone <- function(obj) {

      obj$info$nmin         <- 4

      obj$data<-data.frame(power=obj$options$power)
      obj$data$n          <- obj$options$propone_n
      obj$data$p1          <- obj$options$propone_p1
      obj$data$p2          <- obj$options$propone_p2
      .common_proportions(obj)

}

.checkdata.proppaired <- function(obj) {

      obj$info$nmin         <- 8

      obj$data<-data.frame(power=obj$options$power)
      obj$data$n           <- obj$options$proppaired_n
      obj$data$alternative <- obj$options$alternative
      obj$data$sig.level   <- obj$options$sig.level
      
      obj$data$p1          <- obj$options$proppaired_p1
      obj$data$p2          <- obj$options$proppaired_p2
      
      if (obj$aim == "es") obj$data$p2<-1

      if (!is.something(obj$data$p1)) 
           obj$stop("P21  is required")

      if (obj$data$p1 >= obj$data$p2)
           obj$stop("P21  should be smaller than P12")

      switch(obj$options$es_type,
             dif = {
                   obj$data$es          <- obj$data$p2-obj$data$p1
                   obj$info$letter      <- greek_vector["Delta"] 
                   obj$info$esmax       <-  .5-obj$data$p1
                   obj$info$esmin       <-  1e-04
                   obj$info$toaes       <- function(data) {
                                               p2   <- data$es+data$p1
                                               p2 / data$p1
                                        }
                   obj$info$fromaes          <- function(data)  data$p2 - data$p1

                    },
             {
                   obj$data$es          <-(obj$data$p2/obj$data$p1)
                   obj$info$letter      <- "Odd"
                   obj$info$esmax       <-  (1-obj$data$p1)/obj$data$p1
                   obj$info$esmin       <-  1.0002
                   obj$info$loges            <-  function(x) x > 10
                   obj$info$toaes            <- function(data) {
                                               p1 <- data$p2/data$es
                                               data$p2/p1
                                              }
                   obj$info$fromaes            <- function(data)  (data$psi)
                                              
             }
      )
    
      if (obj$aim != "es") {
        if ( (obj$data$p1+obj$data$p2) > 1 ) {
          obj$stop("The sum of discordand proportion (P21+P12)  should be less than 1.")
        }
        if ( obj$data$es < obj$info$esmin ) {
          obj$stop("Proportions are almost identical. Power parameters cannot be computed.")
        }
        
      }
      obj$data[[obj$aim]]<-NULL
      
}


.common_proportions<-function(obj) {

        obj$data$sig.level   <- obj$options$sig.level
        obj$data$alternative <- obj$options$alternative
        
        obj$info$esmin        <- .001

      if (!is.something(obj$data$p1)) 
           stop("P1  is required")
      if (obj$data$p1>.999999) {
              obj$warning<-list(topic="issues",
                                message="P1 should be less than 1",
                                head="error"
                               )
              obj$ok <- FALSE
        
      } 


      if (is.something(obj$data$p2)) {
          if (obj$data$p2 > obj$data$p1)  {
              p1<- obj$data$p1
              obj$data$p1 <- obj$data$p2
              obj$data$p2 <- p1
              obj$warning<-list(topic="issues",
                                message="P1 is supposed to be larger than P2. Proportions are recomputed to yield equivalent results.",
                                head="warning"
                               )
          }
      }
    
      switch(obj$options$es_type,
             odd = {
                   obj$data$es          <-(obj$data$p1/(1-obj$data$p1))/(obj$data$p2/(1-obj$data$p2))
                   obj$info$letter      <- "Odd"
                   obj$info$esmax       <-  10^5
                   obj$info$esmin       <-  1
                   obj$info$loges            <-  function(x) x > 10        
                   obj$info$toaes            <- function(data) {
                                               odd2 <- (data$p1/(1-data$p1))/data$es
                                               p2   <- odd2/(1+odd2)
                                               pwr::ES.h(data$p1,p2)
                                              }
                   obj$info$fromaes            <- function(data)  (data$p1/(1-data$p1))/(data$p2/(1-data$p2))
                                              
             },
             dif = {
               
                   obj$data$es          <- obj$data$p1-obj$data$p2
                   obj$info$letter      <- greek_vector["Delta"] 
                   obj$info$esmax       <-  obj$data$p1
                   obj$info$esmin       <-  .0001
                   obj$info$toaes            <- function(data) {
                                               p2   <- data$p1-data$es
                                               pwr::ES.h(data$p1,p2)
                                        }
                   obj$info$fromaes          <- function(data)  data$p1 - data$p2

                    },
             rr = {

                   obj$data$es<- obj$data$p1/obj$data$p2
                   obj$info$letter      <- "RR" 
                   obj$info$esmax       <-  10^5
                   obj$info$esmin       <-  1
                   obj$info$loges            <-  function(x) x > 10        
                   
                   obj$info$toaes            <- function(data) {
                                               p2   <-  data$p1/data$es
                                               pwr::ES.h(data$p1,p2)
                                          }
                   obj$info$fromaes          <- function(data)  data$p1 / data$p2

                    }
               
      )
    
      
      if (is.something(obj$data$p1) && obj$data$p1<0.0001) {
                    obj$warning<-list(topic="issues",
                                message="Proportions should be larger than .0001.",
                                head="error"
                               )
                    obj$ok <- FALSE
  
      }
      if (is.something(obj$data$p2) && obj$data$p2<0.0001) {
                    obj$warning<-list(topic="issues",
                                message="Proportions should be larger than .001.",
                                head="error"
                               )
                    obj$ok <- FALSE
        
      }
        if (is.something(obj$data$p2) && obj$data$p2==obj$data$p1) {
                    obj$warning<-list(topic="issues",
                                message="Proportions cannot be equal (null power)",
                                head="error"
                               )
                    obj$ok <- FALSE
  
        }

      obj$data[[obj$aim]]<-NULL

}

## powervector:     (required) pass the data, with adjutment, to the lowerlevel power functions 

.powervector.propind <- function(obj,data) {
  
                 aim <- required_param(data)

                if (!is.null(data$es)) {
                  data$h<-obj$info$toaes(data)
                  data$es<-NULL
                } else {
                  data$p1-NULL
                  data$p2<-NULL
                }
                if (is.null(data$n)) {
                  data$n1<-NULL
                  data$n2<-NULL
                } 
               .names <- intersect(names(data),rlang::fn_fmls_names(pamlj.propind))
                data$alternative<-as.character(data$alternative)
                
                if (hasName(data,"n")) {
                  data$n1 <- data$n/(1+data$n_ratio)
                  data$n1[data$n1<2]<-2
                  data$n2 <- data$n1*data$n_ratio
                }
                results<-lapply(1:nrow(data),function(i) {
                     one<-as.list(data[i,.names])
                     tryobj<-try_hard(do.call(pamlj.propind,one),silent=TRUE)
                     res<-tryobj$obj
                     if (!isFALSE(tryobj$error)) {
                       switch(aim,
                              n={
                                 if (round(one$n_ratio*obj$info$nmin) < 2)
                                       n1<-round(obj$info$nmin/one$n_ratio)
                                 else 
                                       n1<-obj$info$nmin/(1+one$n_ratio)
                                res<-list(n1=n1,n2=n1*one$n_ratio,h=one$h,sig.level=one$sig.level,power=one$power,alternative=one$alternative,method="nmin")
                              },
                              es={
                                mark("error in low function for es")
                              }
                              )
                     }
                     res
                    })
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 odata<- data[, !names(data) %in% names(results)]
                 results<-cbind(odata,results)
             
                 results$n  <- results$n1 + results$n2
                 tp2 <-   (2 * asin(sqrt(results$p1))) - results$h 
                 results$p2 <- sin(tp2/2)^2
                 results$es <- obj$info$fromaes(results)
                 
                 results$es[tp2<0]<-obj$info$esmax           
                 results$h  <- NULL
                 
                return(results)
}


.powervector.propone <- function(obj,data) {
                
                aim <- required_param(data)
              
                if (!is.null(data$es)) {
                  data$h<-obj$info$toaes(data)
                  data$es<-NULL
                } else {
                  data$p1-NULL
                  data$p2<-NULL
                }
                .names <- intersect(names(data),rlang::fn_fmls_names(pwr::pwr.p.test))
                 data$alternative<-as.character(data$alternative)
                 results<-lapply(1:nrow(data),function(i) {
                     one<-as.list(data[i,.names])
                     tryobj<-try_hard(do.call(pwr::pwr.p.test,one))
                     out<-tryobj$obj
                     if (!isFALSE(tryobj$error)) {
                       res<-one
                       switch(aim,
                              n={
                                out<-list(h=one$h,n=obj$info$nmin,sig.level=one$sig.level,power=one$power,alternative=one$alternative,method="nmin")
                               },
                              es={
                                mark("error in low function for es")
                              },
                              power={
                                mark("error in low function for power")
                              }

                              )
                     }
                     out

                    })
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 .names<-c(names(data)[!names( data) %in% names(results)],names(results))
                 odata<- data[, !names( data) %in% names(results)]
                 results<-cbind(odata,results)
                 names(results)<-.names

                 tp2 <-   (2 * asin(sqrt(results$p1))) - results$h 
                 results$p2 <- sin(tp2/2)^2
                 results$es <- obj$info$fromaes(results)
                 results$es[tp2<0]<-obj$info$esmax           
                 results$aprox[results$n < obj$info$nmin]<-"nsmall"
                 results$n[results$n < obj$info$nmin] <- obj$info$nmin
                
                 
            
                 results$n1<-NA
                 results$n2<-NA
                 results$h  <- NULL
                return(results)
}


.powervector.proppaired <- function(obj,data) {
  
                aim <- required_param(data)
    
                if (!is.null(data$es)) {
                  data$psi<-obj$info$toaes(data)
                  data$es<-NULL
                } 
 
                 data$method<-"normal"
                .names <- intersect(names(data),rlang::fn_fmls_names(pamlj.prop.paired))
                 data$alternative<-as.character(data$alternative)
                results<-lapply(1:nrow(data),function(i) {
                     one<-as.list(data[i,.names])
                     tryobj<-try_hard(do.call(pamlj.prop.paired,one), silent=TRUE)
                     out<-tryobj$obj
                     if (!isFALSE(tryobj$error)) {
                       res<-one
                       switch(aim,
                              n={
                               mark("error in low function for n")
                          
                                out<-list(n=obj$info$nmin,p1=one$p1,psi=one$psi,sig.level=one$sig.level,power=one$power,alternative=one$alternative,method="nmin")
                               },
                              es={
                                mark("error in low function for es")
                                  out<-list(n=one$n,p1=one$p1,psi=NA,sig.level=one$sig.level,power=one$power,alternative=one$alternative,method="eserror")

                              },
                              power={
                                mark("error in low function for power")
                                out<-list(n=one$n,p1=one$p1,psi=one$psi,sig.level=one$sig.level,power=NA,alternative=one$alternative,method="error")

                              }

                              )
                     }
                     out
                    })
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                .names<-c(names(data)[!names( data) %in% names(results)],names(results))
                 odata<- data[, !names( data) %in% names(results)]
                 results<-cbind(odata,results)
                 names(results)<-.names

                 results$p2 <- results$p1*results$psi
                 results$es <- obj$info$fromaes(results)
                 results$n1<-NA
                 results$n2<-NA
                 results$psi  <- NULL
                return(results)
}

## powertab_init:   (not required) this function produces or format the main table, powertab, before running
## powertab_run:    (not required) this function produces or format the main table, powertab, after running
## NO NEED

## powerbyes:       this function produces or format the powerbyes table , after the estimation is done
## NO NEED

## powerbyen:       this function produces or format the powerbyes table , after the estimation is done
## NO NEED

## effectsize_init: (not required) some sub.module requires additional effect size to be computed. This function inits the table
## effectsize_run:  (not required) some sub.module requires additional effect size to be computed. This function fills the table
## NO NEED

## extrainfo:       (better if is there) this function add some extra info to be given to the user when the option "explain" is selected


.extrainfo.propind <- function(obj) {
 
   if (!obj$option("explain")) return()  
  
   infoparms<-list(n="total sample size N=" %+% obj$data$n,
                   es="effect size " %+% obj$info$letter %+% " = " %+% format5(obj$data$es),
                   power="power equal to " %+% obj$data$power)
   
   infoparms[[obj$aim]]<-NULL

   text<-"<p> Power parameters are computed for a z-test with " %+%
          paste(infoparms, collapse=", ") %+% 
          " and type I error rate set to " %+% obj$data$sig.level %+%
          " The z-test compares two proportions in two different groups of cases." %+%
          " The proportions are transformed with the archsine function before computing the power parameters.</p>" %+%
            "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format5(obj$data[[obj$aim]]) %+% ".</p>"

    obj$warning<-list(topic="extrainfo",message=text,head="info")  

}

.extrainfo.propone <- function(obj) {
 
   if (!obj$option("explain")) return()  
  
   infoparms<-list(n="total sample size N=" %+% obj$data$n,
                   es="effect size " %+% obj$info$letter %+% " = " %+% format5(obj$data$es),
                   power="power equal to " %+% obj$data$power)
   
   infoparms[[obj$aim]]<-NULL

   text<-"<p> Power parameters are computed for a z-test with " %+%
          paste(infoparms, collapse=", ") %+% 
          " and type I error rate set to " %+% obj$data$sig.level %+%
          " The z-test tests one proportion (P1) obtained in the one sample against an hypothetical value (P2)." %+%
          " The proportions are transformed with the archsine function before computing the power parameters.</p>" %+%
            "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format5(obj$data[[obj$aim]]) %+% ".</p>"

    obj$warning<-list(topic="extrainfo",message=text,head="info")  

}

.extrainfo.proppaired <- function(obj) {
 
   if (!obj$option("explain")) return()  
  
   infoparms<-list(n="total sample size N=" %+% obj$data$n,
                   es="effect size " %+% obj$info$letter %+% " = " %+% format5(obj$data$es),
                   power="power equal to " %+% obj$data$power)
   
   infoparms[[obj$aim]]<-NULL

   text<-"<p> Power parameters are computed for a (approximated) McNemar test with " %+%
          paste(infoparms, collapse=", ") %+% 
          " and type I error rate set to " %+% obj$data$sig.level %+%
          " The McNemar tests two proportions obtained in the same sample." %+%
            "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format5(obj$data[[obj$aim]]) %+% ".</p>"

    obj$warning<-list(topic="extrainfo",message=text,head="info")  

}
