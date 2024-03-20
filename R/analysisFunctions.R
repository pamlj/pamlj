## here are functions that are always called but return different information
## depending to the analysis being carried out

checkdata <- function(obj, ...) UseMethod(".checkdata")

.checkdata.correlation <- function(obj) {
  
  if (is.something(obj$data$es)) {
     if (abs(obj$data$es)<.001)
         stop("Power parameters for correlation absolute value less than .001 cannot be computed.")
    
     if (abs(obj$data$es)==1) {
         obj$warning<-list(topic="powertab",massage="")
         obj$warning<-list(topic="powertab",message="Power parameters for correlation absolute value equal to 1 cannot be computed. The effect
                           size is set to .999, which yields equivalent power values.")
         obj$data$es<-.999
     }
     if (abs(obj$data$es)>1) {
         stop("correlation absolute value cannot be larger than 1.")
     }

     }
      obj$data[["aes"]] <- obj$data$es
      obj$data$letter   <- greek_vector["rho"]

}

.checkdata.beta <- function(obj) {
  
    obj$data$letter   <- greek_vector["beta"]


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

  obj$data$letter   <- letter_peta2
  
  if (is.something(obj$data$es)) {
     if (abs(obj$data$es)<.001)
         stop("Partial Eta-squared value cannot be less than .001")
     if (abs(obj$data$es)>.99)
         stop("Partial Eta-squared value cannot be more than .99")
  }
  
  if (is.something(obj$data$df_model)) {
                if (obj$data$df_model < 1)
                           stop("Model degrees of freedom cannot be less than 1")
    } else {
        stop("GLM power analysis based on partial eta-squared requires the expected degrees of freedom of the model")
    }
  
    if (is.something(obj$data$df_effect)) {
                if (obj$data$df_effect < 1)
                           stop("Effect degrees of freedom cannot be less than 1")
    } else {
        stop("GLM power analysis based on partial eta-squared requires the expected degrees of freedom of the effect")
    }

    if ( obj$data$df_model < obj$data$df_effect ) {
           obj$data$df_model <- obj$data$df_effect
           obj$warning<-list(topic="powertab",message="Model degrees of freedom cannot be less than the effect degrees of freedom. 
                                                   They have been set equal. ")
    }

    if (!is.something(obj$data[["es"]]))
       return()

    obj$data[["aes"]] <- obj$data$es/(1-obj$data$es)


}

.checkdata.eta <- function(obj) {

  jinfo("MODULE:  PAMLglm eta  #### check data  ####")
  obj$data$letter   <- letter_eta2
  
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

