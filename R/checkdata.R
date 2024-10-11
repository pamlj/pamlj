### common checks on data

checkfailure <- function(obj, ...) UseMethod(".checkfailure")

.checkfailure.default <- function(obj,results) {


}


### additional check for models


commonchecks <- function(obj) {
  
    if (!obj$ok) return()
    jinfo("PAMLj: performing common checks")

    if (is.something(obj$data$sig.level ) ) {
        if ( any(obj$data$sig.level < 0.00001) ||  any(obj$data$sig.level > .90)) {
                   obj$stop("Type I error rate should be between .00001 and .90")
                   return()

        }
    } else {
        obj$stop("Power analysis requires Type I rate")
    }
  
    if (is.something(obj$data$power ) ) {
        if ( any(obj$data$power < 0.10) ||  any(obj$data$power > .99)) {
                   obj$stop("Power should be between .10 and .99")
                   
        }
        if ( any(obj$data$power < obj$data$sig.level) ) {
                   obj$stop("Power should be larger than Type I error rate ")
        }

    } 

    if (is.something(obj$data[["n"]] ) ) {
        if ( any(obj$data$n < obj$info$nmin )) {
                   obj$stop(paste("N (total sample size) should be larger than",obj$info$nmin))
        }
    } 
  
    ## use the [[ ]] because of the crazy R partial matching
    if (is.something(obj$data[["es"]])) {
      
    if (is.something(obj$info$eslbound))
         if (obj$info$eslbound > obj$data$es) obj$stop("The effect size " %+% obj$info$letter %+% " cannot be smaller than " %+% obj$info$eslbound)

  
    if (is.something(obj$info$esmax) )
         if (obj$info$esmax < obj$data$es) obj$stop("The effect size " %+% obj$info$letter %+% " cannot be larger than " %+% obj$info$esmax)

    if (!obj$option("is_equi") ) {
      
        .data<-obj$data
        .data$n<-obj$info$nmax
        esmin<-find_min_es(obj,.data)
        fesmin<-format5(esmin)
        es<-format5(obj$data$es)


         if ( any(abs(obj$data[["es"]]) < abs(esmin))) {
                   message<-       "The effect size (" %+% obj$info$letter %+% " = " %+% es %+% ") is smaller than the effect size (" %+%
                                    obj$info$letter %+% " = " %+% fesmin %+% ")" %+%
                                   " that requires around " %+% obj$info$nmax_spell %+% " cases (N=" %+% obj$info$nmax %+% ")  to obtain a power of " %+%
                                    obj$data$power %+% ". Results are shown for " %+% obj$info$letter %+% " = " %+% esmin %+%". " %+%
                                    "Sensitivity analysis (plots) cannot be produced." %+% 
                                    "<a href='https://pamlj.github.io/details_failure.html' target='_blank'> More info here </a>"
                   obj$warning<-list(topic="issues",message=message,head="warning")
                   obj$data$es<-esmin
                   obj$plots$sensitivity<-FALSE
                   
         }
    }
       
      
    } 
  
    jinfo("PAMLj: performing common checks done")

}




postchecks<-function(obj) {
  
  jinfo("PAMLj: post checks")
  
    data<-obj$data
    switch (data$method,
      nmin = {   
              data$n <- obj$info$nmin
              esmax  <- find_max_es(obj,data)
              es     <- round(data$es,digits=5) 
              
              message<-    "The effect size ("%+% obj$info$letter %+% " = " %+%  es %+% ") is larger than the maximum effect size (" %+% obj$info$letter %+% 
                                   "=" %+% format(esmax,digits=5) %+% ")" %+%
                                   " that guarantees power=" %+% data$power %+% " with a sample of minimum size (N=" %+% data$n %+% ")." %+%
                                   "This means that any effect size larger than "%+% format(esmax,digits=5) %+% " guarantees a power > " %+% data$power %+% 
                                   " for any sample size equal or larger than " %+% data$n %+% "." %+%
                                   "<a href='https://pamlj.github.io/details_failure.html' target='_blank'> More info here </a>"
              obj$warning<-list(topic="issues",message=message,head="info")
              },
      brute = {   
              es     <- format5(data$es) 
              message<-"The effect size (" %+% obj$info$letter %+% " = " %+%  es %+% ") is very small. An approximate method is used to compute the effect size. " %+%
                        "Sensitivity analysis (and plots) cannot be computed." 
              obj$warning<-list(topic="issues",message=message,head="warning")
              obj$plots$sensitivity<-FALSE
              },
      powmax = {   
              es     <- format5(data$es) 
              power  <- format5(obj$data$power)
              message<-"Given the input parameters, a power equal to " %+% obj$options$power %+% " cannot be achieved." %+%
                       " The maximum feasable effect size (" %+% obj$info$letter %+% " = " %+%  es %+% ") yields a power of " %+% power %+% ". " %+%
                        "Sensitivity analysis (and plots) cannot be computed." 
              obj$warning<-list(topic="issues",message=message,head="warning")
              obj$plots$sensitivity<-FALSE

        },
      eserror = {   

              message<-"The required effect size (" %+% obj$info$letter %+% ") cannot be computed. It's value is likely larger than a feasable effect size." %+%
                        "Sensitivity analysis (and plots) cannot be computed." 
              obj$warning<-list(topic="issues",message=message,head="warning")
              obj$ok<-FALSE
              }
    ) # end of switch
    
      if ( any(obj$data$n < obj$info$nmin )) {

              message<-"The sample size  (N=" %+% round(obj$data$n, digits=0) %+% ") is too small. It's value is likely not feasable in actual research. " %+%
                        "Sensitivity analysis (and plots) may be biased." 
              obj$warning<-list(topic="issues",message=message,head="warning")

        }


  
  if (obj$data$power>.9999) {
    
                   message<-paste0("The analysis reports a power close to 1.  Sensitivity analyses may be not accurate.")
                   obj$warning<-list(topic="plotnotes",message=message,head="warning")
    
  }
  if (obj$data$n < obj$info$nsave) {
    
                   message<-paste0("The analysis yields a very small sample size.  Sensitivity analyses may be not accurate.")
                   obj$warning<-list(topic="plotnotes",message=message,head="warning")
    
  }

  morechecks(obj)
}

morechecks <- function(obj, ...) UseMethod(".morechecks")

.morechecks.default <- function(obj) return()


.morechecks.glm <- function(obj) {
  
  jinfo("PAMLj: more checks glm")

  if (obj$aim == "es") {
    
    if (obj$data$df_model==1) obj$info$r2<-obj$data$f2/(1+obj$data$f2)
    
  }

}


.morechecks.mediation <- function(obj) {
  
  jinfo("PAMLj: more checks mediation")

  if (obj$aim == "es") {
                   message<-"The analysis seeks for X to Mediator coefficient  (a) that guarantees (if possible) the required power given the sample size and sig.level"
                   obj$warning<-list(topic="issues",message=message,head="info")
  }
  
  
  if (1==2 && is.something(obj$extradata)) {
    
  .obj<-obj
   class(.obj)<-class(obj)[-length(class(obj))]

   for (i in seq_len(nrow(obj$extradata))) {
                   .obj$data<-.obj$extradata[i,]
                   commonchecks(.obj)
   }
  } else {
    
    obj$info$rxy<-obj$data$a*obj$data$b+obj$data$cprime
    
  }
  
}

