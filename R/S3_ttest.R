## S3 functions for ttest

### check data ###

.checkdata.ttestind <- function(obj) {

      obj$data<-data.frame(es=obj$options$ttestind_es)
      obj$data$n1          <- obj$options$ttestind_n
      obj$data$n_ratio      <- obj$options$ttestind_nratio
      obj$data$n2          <- obj$data$n1*obj$data$n_ratio
      obj$data$n           <- obj$data$n1+obj$data$n2
      obj$data$sig.level   <- obj$options$sig.level
      obj$data$power       <- obj$options$power
      obj$data$alternative <- obj$options$alternative
      obj$data[[obj$aim]]  <- NULL
      
      obj$info$letter      <- greek_vector["delta"]
      obj$info$esmax       <- 20
      obj$info$esmin       <- 0.001


      if (obj$options$is_equi ) {
        
        if (abs(obj$options$equi_limit)>0) {
        obj$info$equi_limit <- obj$options$equi_limit
        obj$info$toaes<- function(value) abs(value-obj$info$equi_limit)
        obj$info$fromaes<- function(value) abs(value-obj$info$equi_limit)
        obj$info$esmin       <-  0
        obj$info$esmax       <-  obj$data$equi_limit-.0001
        }
        if (obj$options$equi_limit<obj$data$es) 
          obj$stop("Equivalence tests require the expected effect size to be smaller than the equivalence limit")
         if (obj$options$equi_limit==0) 
          obj$stop("Equivalence tests require the equivalence limit to be larger than 0")
      }

}

# DC

.checkdata.ttestpaired <- function(obj) {

      obj$data<-data.frame(es=obj$options$ttestpaired_es)
      obj$data$n           <- obj$options$ttestpaired_n
      obj$data$sig.level   <- obj$options$sig.level
      obj$data$power       <- obj$options$power
      obj$data$alternative <- obj$options$alternative
      obj$data$type        <- "paired"
      obj$data[[obj$aim]]  <- NULL
      
      obj$info$letter      <- greek_vector["delta"]
      obj$info$esmax       <-  10
      obj$info$esmin       <- .01
      obj$info$nmin        <- 3
      if (obj$options$is_equi ) {

        if (abs(obj$options$equi_limit)>0) {
        obj$info$equi_limit <- obj$options$equi_limit
        obj$info$toaes<- function(value) abs(value-obj$info$equi_limit)
        obj$info$fromaes<- function(value) abs(value-obj$info$equi_limit)
        obj$info$esmin       <-  0
        obj$info$esmax       <-  obj$data$equi_limit-.0001
        }
        if (obj$options$equi_limit<obj$data$es) 
          obj$stop("Equivalence tests require the expected effect size to be smaller than the equivalence limit")
         if (obj$options$equi_limit==0) 
          obj$stop("Equivalence tests require the equivalence limit to be larger than 0")

      }

      
      
}

# DC

.checkdata.ttestone <- function(obj) {

      obj$data<-data.frame(es=obj$options$ttestone_es)
      obj$data$n           <- obj$options$ttestone_n
      obj$data$sig.level   <- obj$options$sig.level
      obj$data$power       <- obj$options$power
      obj$data$alternative <- obj$options$alternative
      obj$data$type        <- "one.sample"
      obj$data[[obj$aim]]  <- NULL
      
      obj$info$letter      <- greek_vector["delta"]
      obj$info$esmax       <- 2.5
      obj$info$esmin       <- .01
      obj$info$nmin        <- 3
      if (obj$options$is_equi ) {
        
        if (abs(obj$options$equi_limit)>0) {
        obj$info$equi_limit <- obj$options$equi_limit
        obj$info$toaes<- function(value) abs(value-obj$info$equi_limit)
        obj$info$fromaes<- function(value) abs(value-obj$info$equi_limit)
        obj$info$esmin       <-  0
        obj$info$esmax       <-  obj$data$equi_limit-.0001
        }
        if (obj$options$equi_limit<obj$data$es) 
          obj$stop("Equivalence tests require the expected effect size to be smaller than the equivalence limit")
         if (obj$options$equi_limit==0) 
          obj$stop("Equivalence tests require the equivalence limit to be larger than 0")

      }
}

### power vector ###


.powervector.ttestind <- function(obj,data) {
  
                aim<-required_param(data)
                
                if (is.something(data$es))
                    data$d <- obj$info$toaes(data$es)
                
                if (is.something(obj$info$equi_limit)) {
                    if (is.something(data$power)) data$power<-(1+data$power)/2
                     data$sig.level<- data$sig.level*2
                }
                
                 data$alternative<-as.character(data$alternative)
           
                if (hasName(data,"n")) {
                  data$n1 <- data$n/(1+data$n_ratio)
                  data$n2 <- data$n1*data$n_ratio
                } else {
                    data$n1<-NULL
                    data$n2<-NULL
                }
                .names <- intersect(names(data),rlang::fn_fmls_names(pamlj.ttestind))
                results<-lapply(1:nrow(data),function(i) {
                     one<-as.list(data[i,.names])
                     tryobj<-try_hard(do.call(pamlj.ttestind,one),silent=TRUE)
                     out<-tryobj$obj
                     if (!isFALSE(tryobj$error)) {
                 
                        switch(aim,
                                n = {
                                     n<-obj$info$nmin
                                     n1<-ceiling(n/(1+data$n_ratio))
                                     n2<-n1*one$n_ratio
                                     out<-data.frame(n1=n1,n2=n2,d=one$d,sig.level=one$sig.level,power=one$power,alternative=one$alternative,method="nmin")
                               }
                             )
                     }
                    out
                    })
             
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 odata<-data[, !names(data) %in% names(results)]
                 results<-cbind(odata,results)
                 results$n  <- results$n1 + results$n2
                 results$df <- results$n - 2
                 results$es <- obj$info$fromaes(results$d)
                 if (is.something(obj$data$equi_limit)) {
                    results$power<-2*results$power-1
                    results$sig.level<-results$sig.level/2
                 }
                 results$power[results$power<0] <- NA
                 results$d  <- NULL
                return(results)
}

#do.call(rbind,list(pwr::pwr.t.test(n=10,d=1)))

.powervector.ttestpaired <- function(obj,data) {
                
                aim<-required_param(data)
                if (is.something(data$es))
                     data$d <- obj$info$toaes(data$es)
                if (is.something(obj$info$equi_limit)) {
                    if (is.something(data$power)) data$power<-(1+data$power)/2
                    data$sig.level <- data$sig.level*2
                }
                .names <- intersect(names(data),rlang::fn_fmls_names(pwr::pwr.t.test))
                 data$alternative<-as.character(data$alternative)
                results<-lapply(1:nrow(data),function(i) {
                     one<-as.list(data[i,.names])
                     one$type <-as.character(one$type)
                     tryobj<-try_hard(do.call(pwr::pwr.t.test,one),silent=TRUE)
                     out<-tryobj$obj
                     if (!isFALSE(tryobj$error)) {
                 
                        switch(aim,
                                n = {
                                     n<-obj$info$nmin
                                     out<-data.frame(n=n,d=one$d,sig.level=one$sig.level,power=one$power,alternative=one$alternative,method="nmin")
                               }
                             )
                     }
                    out
                    })                
                 results<-as.data.frame(do.call("rbind",results))
                 results$note<-NULL
              

                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])

                 odata<-subset(data, select= !(names(data) %in% names(results)))
                 results<-cbind(odata,results)
                 results$n  <- ceiling(results$n)
                 results$df <- results$n - 1
                 results$es <- obj$info$fromaes(results$d)
                 if (is.something(obj$info$equi_limit)) {
                    results$power<-2*results$power-1
                    results$sig.level<-results$sig.level/2
                 }
                 results$power[results$power<0] <- NA
                 results$d  <- NULL

                return(results)
}

.powervector.ttestone <- function(obj,data) {
                
  return(.powervector.ttestpaired(obj,data))
}

### powerbyes table function

.powerbyes.ttest <- function(obj) {


  
            if (!obj$option("is_equi"))
               return(.powerbyes.default(obj))
  
            probs = c(.5, .8, .95)
            .data<-obj$data
            .data$es<-NULL
            probs_es = sapply(probs, function(p){
              .data$power<-p
               rr<-try_hard(powervector(obj,.data))
               if (isFALSE(rr$error))
                   return(rr$obj$es)
               else
                   return(NA)
           })
            check<-which(is.na(probs_es))
            if (length(check)>0) warning("Some effect size cannot be computed given the input parameters.")
            
            probs_es<-format(probs_es,digits=3)
            esList <-list(list(es=paste(obj$info$letter, ">",probs_es[1])),
                          list(es=paste(probs_es[1],greek_vector["geq"], obj$info$letter, ">",probs_es[2])),
                          list(es=paste(probs_es[2],greek_vector["geq"], obj$info$letter, ">",probs_es[3])),
                          list(es=paste(obj$info$letter, greek_vector["leq"],probs_es[3]))
            )
            attr(esList,"titles")<-list(power="Power for equivalence")

            return(esList)
            
}


### this function add some extra info to be given to the user when the option "explain" is selected


.extrainfo.ttestind <- function(obj) {
 
   if (!obj$option("explain")) return()  
  
   infoparms<-list(n="total sample size N=" %+% obj$data$n,
                   es="effect size " %+% obj$info$letter %+% " = " %+% format5(obj$data$es),
                   power="power equal to " %+% obj$data$power)
   
   infoparms[[obj$aim]]<-NULL

   text<-"<p> Power parameters are computed for a t-test with " %+%
          paste(infoparms, collapse=", ") %+% 
          " and type I error rate set to " %+% obj$data$sig.level %+%
          " The t-test compares the means of two groups of cases.</p>" %+%
            "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format5(obj$data[[obj$aim]]) %+% ".</p>"

    obj$warning<-list(topic="extrainfo",message=text,head="info")  

}

.extrainfo.ttestpaired <- function(obj) {
  
   if (!obj$option("explain")) return()  
       
   infoparms<-list(n="total sample size N=" %+% obj$data$n,
                   es="effect size " %+% obj$info$letter %+% " = " %+% format5(obj$data$es),
                   power="power equal to " %+% obj$data$power)
   
   infoparms[[obj$aim]]<-NULL

   text<-"<p> Power parameters are computed for a t-test with " %+%
          paste(infoparms, collapse=", ") %+% 
          " and type I error rate set to " %+% obj$data$sig.level %+%
          " The t-test compares two means in a repeated-measure design.</p>" %+%
            "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format5(obj$data[[obj$aim]]) %+% ".</p>"
      obj$warning<-list(topic="extrainfo",message=text,head="info")  
}

.extrainfo.ttestone <- function(obj) {
  
   if (!obj$option("explain")) return()  

   infoparms<-list(n="total sample size N=" %+% obj$data$n,
                   es="effect size " %+% obj$info$letter %+% " = " %+% format5(obj$data$es),
                   power="power equal to " %+% obj$data$power)
   
   infoparms[[obj$aim]]<-NULL

   text<-"<p> Power parameters are computed for a t-test with " %+%
          paste(infoparms, collapse=", ") %+% 
          " and type I error rate set to " %+% obj$data$sig.level %+%
          " The t-test tests the mean of one sample against the value of zero.</p>" %+%
            "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format5(obj$data[[obj$aim]]) %+% ".</p>"
      obj$warning<-list(topic="extrainfo",message=text,head="info")  
}
