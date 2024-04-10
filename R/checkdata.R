## here are functions that are always called but return different information
## depending to the analysis being carried out

checkdata <- function(obj, ...) UseMethod(".checkdata")

.checkdata.ttestind <- function(obj) {

      obj$data$letter      <- greek_vector["delta"]
      obj$data$es          <- obj$options$ttestind_es
      obj$data$n1          <- obj$options$ttestind_n
      obj$data$n_ratio      <- obj$options$ttestind_nratio
      obj$data$n2          <- obj$data$n1*obj$data$n_ratio
      obj$data$n           <- obj$data$n1+obj$data$n2
      obj$nmin             <- 6
      obj$data$alternative <- obj$options$alternative
      obj$data$esmax       <- 2.5
      obj$data$esmin       <- .01

}

.checkdata.ttestpaired <- function(obj) {

      obj$data$letter      <- greek_vector["delta"]
      obj$data$es          <- obj$options$ttestpaired_es
      obj$data$aes         <- obj$data$es
      obj$data$n           <- obj$options$ttestind_n
      obj$nmin             <- 6
      obj$data$alternative <- obj$options$alternative
      obj$data$type        <- "paired"
      obj$data$esmax       <- 2.5
      obj$data$esmin       <- .01

}

.checkdata.ttestone <- function(obj) {

      obj$data$letter      <- greek_vector["delta"]
      obj$data$es          <- obj$options$ttestpaired_es
      obj$data$aes         <- obj$data$es
      obj$data$n           <- obj$options$ttestind_n
      obj$nmin             <- 6
      obj$data$alternative <- obj$options$alternative
      obj$data$type        <- "one.sample"
      obj$data$esmax       <- 2.5
      obj$data$esmin       <- .01

}

### correlation ####

.checkdata.correlation <- function(obj) {

      obj$data$letter      <- greek_vector["rho"]
      obj$data$es          <- obj$options$es
      obj$data$aes         <- obj$data$es
      obj$data$n           <- obj$options$n
      obj$nmin             <- 6
      obj$data$alternative <- obj$options$alternative
      obj$data$esmax       <-  .99
      obj$data$esmin       <- .01
      


}

### proportions ###
.checkdata.propind <- function(obj) {

      obj$data$n1          <- obj$options$propind_n
      obj$data$n_ratio     <- obj$options$propind_nratio
      obj$data$n2          <- ceiling(obj$data$n1 * obj$data$n_ratio)
      obj$data$n           <- obj$data$n1 + obj$data$n2

      obj$nmin             <- 6
      obj$data$p1          <- obj$options$propind_p1
      obj$data$p2          <- obj$options$propind_p2
      .common_proportions(obj)


}

.checkdata.propone <- function(obj) {

      obj$data$n          <- obj$options$propone_n
      obj$nmin             <- 6
      obj$data$p1          <- obj$options$propone_p1
      obj$data$p2          <- obj$options$propone_p2
      .common_proportions(obj)

}

.common_proportions<-function(obj) {

        obj$data$alternative <- obj$options$alternative
     
      if (!is.something(obj$data$p1)) 
           stop("P1  is required")

      if (is.something(obj$data$p2)) {
          if (obj$data$p2 > obj$data$p1)  {
              p1<- obj$data$p1
              obj$data$p1 <- obj$data$p2
              obj$data$p2 <- p1
              obj$warning<-list(topic="issues",
                                message="P1 is supposed to be larger than P2. Proportions are recomputed to yield equivalent results."
                               )
          }
        }
      switch(obj$options$es_type,
             odd = {
                   obj$data$letter      <- "Odd"
                   obj$data$es          <-(obj$data$p1/(1-obj$data$p1))/(obj$data$p2/(1-obj$data$p2))
                   obj$data$esmax       <-  10
                   obj$data$esmin       <-  1
                   obj$loges            <-  TRUE        
                   obj$loges_from       <-  obj$data$esmax        
                   obj$toaes            <- function(data) {
                                               odd2 <- (data$p1/(1-data$p1))/data$es
                                               p2   <- odd2/(1+odd2)
                                               pwr::ES.h(data$p1,p2)
                                              }
                   obj$fromaes            <- function(data)  (data$p1/(1-data$p1))/(data$p2/(1-data$p2))
                                              
             },
             dif = {
                   obj$data$letter      <- greek_vector["Delta"] 
                   obj$data$es          <- obj$data$p1-obj$data$p2
                   obj$data$esmax       <-  obj$data$p1
                   obj$data$esmin       <-  .01
                   obj$toaes            <- function(data) {
                                               p2   <- data$p1-data$es
                                               pwr::ES.h(data$p1,p2)
                                        }
                   obj$fromaes          <- function(data)  data$p1 - data$p2

                    },
             rr = {
                   obj$data$letter      <- "RR" 
                   obj$data$es<- obj$data$p1/obj$data$p2
                   obj$data$esmax       <-  10
                   obj$data$esmin       <-  1
                   obj$toaes            <- function(data) {
                                               p2   <-  data$p1/data$es
                                               pwr::ES.h(data$p1,p2)
                                          }
                   obj$fromaes          <- function(data)  data$p1 / data$p2

                    }
               
      )
    
      
      if (is.something(obj$data$p1) && obj$data$p1<0.001) 
           stop("Proportions cannot be less than 0.001")
      if (is.something(obj$data$p2) && obj$data$p2<0.001) 
           stop("Proportions cannot be less than 0.001")
      if (is.something(obj$data$p2) && obj$data$p2==obj$data$p1) 
           stop("Proportions cannot be equal (null power)")

      if (is.something(obj$data$p2) && obj$data$p2==obj$data$p1) 
           stop("Proportions cannot be equal (null power)")

}

.checkdata.proppaired <- function(obj) {

      obj$data$n          <- obj$options$proppaired_n
      obj$nmin             <- 6
      obj$data$alternative <- obj$options$alternative
      obj$data$p1          <- obj$options$proppaired_p1
      obj$data$p2          <- obj$options$proppaired_p2
      if (obj$aim == "es") obj$data$p2<-1

      if (!is.something(obj$data$p1)) 
           stop("P21  is required")

      if (obj$data$p1 >= obj$data$p2)
           stop("P21  should be smaller than P12")

      switch(obj$options$es_type,
             dif = {
                   obj$data$letter      <- greek_vector["Delta"] 
                   obj$data$es          <- obj$data$p2-obj$data$p1
                   obj$data$esmax       <-  1-obj$data$p1
                   obj$data$esmin       <-  .01
                   obj$toaes            <- function(data) {
                                               p2   <- data$p2-data$es
                                               data$p2/p1
                                        }
                   obj$fromaes          <- function(data)  data$p2 - data$p1

                    },
             {
                   obj$data$letter      <- "Odd"
                   obj$data$es          <-(obj$data$p2/obj$data$p1)
                   obj$data$esmax       <-  (1-obj$data$p1)/obj$data$p1
                   obj$data$esmin       <-  1.01
                   obj$loges            <- TRUE
                   obj$loges_from       <- 10
                   obj$toaes            <- function(data) {
                                               p1 <- data$p2/data$es
                                               data$p2/p1
                                              }
                   obj$fromaes            <- function(data)  (data$psi)
                                              
             }
             

      )
    
      
      if (obj$data$p1<0.001) 
           stop("Proportions cannot be less than 0.001")
      if (obj$data$p1>=0.5) 
           stop("P12 is the smallest discordant probability and must be less than 0.5 ")
      
      if (obj$data$p2<0.001) 
           stop("Proportions cannot be less than 0.001")
      if (obj$data$p2==obj$data$p1) 
           stop("Proportions cannot be equal (null power)")


}

### GLM ###


.checkdata.beta <- function(obj) {
  
      obj$data$letter   <- greek_vector["beta"]
      obj$data$n           <- obj$options$n
      obj$data$es          <- as.numeric(obj$options$b_es)
      obj$data$r2          <- as.numeric(obj$options$b_r2)
      obj$data$df_model    <- obj$options$b_df_model
      obj$data$df_effect   <- 1

      obj$data$df_model    <- obj$options$b_df_model
      obj$data$esmax       <-  .99
      obj$data$esmin       <- .01
      obj$data$alternative <- obj$options$alternative
      obj$nmin             <- obj$data$df_model+10
      obj$logy             <- TRUE
      obj$loges            <- TRUE
      obj$toaes            <- function(value) value^2/(1-obj$data$r2)
      obj$fromaes          <- function(value) sqrt(value*(1-obj$data$r2))  

  if (is.something(obj$data$df_effect) && obj$data$df_effect==0) 
          stop("Effect degrees of freedom cannot be zero")
  
  if (is.something(obj$data$es)) {
     if (abs(obj$data$es)<.001)
         stop("Beta coefficient absolute value cannot be less than .001")
     if (abs(obj$data$es)>.99)
         stop("Beta coefficient absolute value cannot be more than .99")
  }
  
    if (is.something(obj$data$r2 ) ) {
        if ( is.something(obj$data$es) && obj$data$r2+.0001 < obj$data$es^2  )
                   stop("R-squared cannot be less than the square of the beta coefficient")
        if (abs(obj$data$r2)>.99)
                   stop("The R-squared cannot be more than .99")
    } else {
        stop("GLM power analysis based on beta coefficients requires an expected R-squared for the model")
    }
  
    if (is.something(obj$data$df_model)) {
                if (obj$data$df_model < 1)
                           stop("Model degrees of freedom cannot be less than 1")
                if (obj$data$df_model == 1 && is.something(obj$data$es)) {
                           obj$warning<-list(topic="powertab",message=paste("When df=1 the R-square is the square of the beta coefficient."))
                           obj$data$r2<-obj$data$es^2
                }
    } else {
        stop("GLM power analysis based on beta coefficients requires the expected degrees of freedom of the model")
    }
    if (!is.something(obj$data[["es"]]))
       return()

    obj$data[["aes"]] <- obj$data$es^2/(1-obj$data$r2)

}

.checkdata.peta <- function(obj) {

      obj$data$letter      <- letter_peta2
      obj$data$n           <- obj$options$n
      obj$data$es          <- as.numeric(obj$options$v_es)
      obj$data$df_model    <- obj$options$v_df_model
      obj$data$df_effect   <- obj$options$v_df_effect
      obj$data$esmax       <-  .99
      obj$data$esmin       <- .01
      obj$data$alternative <- obj$options$alternative
      obj$nmin             <- obj$data$df_model+10
      obj$logy             <- TRUE
      obj$toaes            <- function(value) value/(1-value)
      obj$fromaes          <- function(value) value/(1+value)  
  
}

.checkdata.eta <- function(obj) {

      obj$data$letter      <- letter_eta2
      obj$data$n           <- obj$options$n
      obj$data$es          <- as.numeric(obj$options$e_es)
      obj$data$r2          <- as.numeric(obj$options$e_r2)

      obj$data$df_model    <- obj$options$e_df_model
      obj$data$df_effect   <- obj$options$e_df_effect
      obj$data$esmax       <-  .99
      obj$data$esmin       <- .01
      obj$data$alternative <- obj$options$alternative
      obj$nmin             <- obj$data$df_model+10
      obj$logy             <- TRUE
      obj$toaes            <- function(value) value/(1-obj$data$r2)
      obj$fromaes          <- function(value) value*(1-obj$data$r2)  

  if (is.something(obj$data$es)) {
     if (abs(obj$data$es)<.001)
         stop("Eta-squared value cannot be less than .001")
     if (abs(obj$data$es)>.99)
         stop("Eta-squared value cannot be more than .99")
  }
  
  if (is.something(obj$data$df_model)) {
                if (obj$data$df_model < 1)
                           stop("Model degrees of freedom cannot be less than 1")
    } else {
        stop("GLM power analysis based on eta-squared requires the expected degrees of freedom of the model")
    }
  
    if (is.something(obj$data$df_effect)) {
                if (obj$data$df_effect < 1)
                           stop("Effect degrees of freedom cannot be less than 1")
    } else {
        stop("GLM power analysis based on eta-squared requires the expected degrees of freedom of the effect")
    }

    if ( obj$data$df_model < obj$data$df_effect ) {
           obj$data$df_model <- obj$data$df_effect
           obj$warning<-list(topic="powertab",message="Model degrees of freedom cannot be less than the effect degrees of freedom. 
                                                   They have been set equal. ")
    }

    if (is.something(obj$data$r2 ) ) {
        if ( is.something(obj$data$es) && obj$data$r2+.0001 < obj$data$es^2  )
                   stop("R-squared cannot be less than the square of the beta coefficient")
        if (abs(obj$data$r2)>.99)
                   stop("The R-squared cannot be more than .99")
    } else {
        stop("GLM power analysis based on Eta-squared coefficient requires an expected R-squared for the model")
    }

  
    if (!is.something(obj$data[["es"]]))
       return()
  
    f2 <-  obj$data$es/(1-obj$data$r2)
    obj$data[["aes"]] <- f2


}


checkfailure <- function(obj, ...) UseMethod(".checkfailure")

.checkfailure.default <- function(obj,results) {
 
  what <- required_param(obj$input)
  nice <- nicify_param(what)
  test<-grep("values at end points not of opposite", results) 
  if (length(test) > 0) {
    message <- paste(nice, " cannot be computed for the combination of input parameters")
    switch (what,
      n = message <- paste(message,"The require power may be too low for or the effect size too large")
    )
   obj$warning<-list(topic="issues",message=message,head="issue")
  
  }
  
}


.checkfailure.proppaired <- function(obj,results) {
 
  what <- required_param(obj$input)
  
  if (what == "es") {
    
    msg<-"A suitable (meaningfull) effect size cannot be found for the given P12 and N"
    data    <-obj$data
    p2      <-1-data$p1
    data$es <-(p2/data$p1)-.0001
    mark(data)
    data$n  <-NULL

    res<-powervector(obj,data)
    n1<- ceiling(res$n)
    data$power=.95
    res<-powervector(obj,data)
    n2<- ceiling(res$n)

    msg<-paste0(msg,". To obtain a meaningful effect size with P12=",data$p1," one needs at lest at least an N=",n1,
                     " for power=",data$power,", and at least N=",n2," for power .95")

    obj$warning<-list(topic="issues",message=msg,head="issue")
    return()   
  }
  
  nice <- nicify_param(what)
  test<-grep("values at end points not of opposite", results) 
  if (length(test) > 0) {
    message <- paste(nice, " cannot be computed for the combination of input parameters")
    switch (what,
      n = message <- paste(message,"The require power may be too low for or the effect size too large")
    )
   obj$warning<-list(topic="issues",message=message,head="issue")
  
  }
  
}
