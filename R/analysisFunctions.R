## here are functions that are always called but return different information
## depending to the analysis being carried out

checkdata <- function(obj, ...) UseMethod(".checkdata")

.checkdata.correlation <- function(obj) {
  
  if (is.something(obj$data$es)) {
     if (abs(obj$data$es)<.001)
         stop("Correlation absolute value cannot be less than .001")
     if (abs(obj$data$es)>.99)
         stop("Correlation absolute value cannot be more than .99")

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
                if (obj$data$df_model == 1) {
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

.checkdata.variance <- function(obj) {

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




text_intro <- function(obj) UseMethod(".text_intro")

.text_intro.correlation <- function(obj) {
  
  text<-" <div>
             <p> Please select the aim of the analysis:</p>
             <ul>
             <li> <b> Calculate N</b> computes the required sample size given the <b> Target effect size</b> and <b> Minimal desire power</b>  </li>          
             <li> <b> Calculate Power</b> computes the achievable power given the <b> Target effect size</b> and <b> N (Sample size)</b>  </li>          
             <li> <b> Calculate Effect Size</b> computes the minimally-detectable effect size given the <b> N (Sample size)</b> and <b> Minimal desire power</b>  </li>          
             <li> <b> Calculate Required alpha</b> it is not very usefull, just let it be there. </li>          
             </ul>
             <p> In all cases, you can set the required Type I error rate and whether the test will be carried out two-tailed or one-tailed.</b>

             </div>

  "
  return(text)

}

.text_intro.beta <- function(obj) {
  
  text<-" <div>
              <p> Please select the aim of the analysis:</p>
             <ul>
             <li> <b> Calculate N</b> computes the required sample size given the <b> Target effect size</b> and <b> Minimal desire power</b>  </li>          
             <li> <b> Calculate Power</b> computes the achievable power given the <b> Target effect size</b> and <b> N (Sample size)</b>  </li>          
             <li> <b> Calculate Effect Size</b> computes the minimally-detectable effect size given the <b> N (Sample size)</b> and <b> Minimal desire power</b>  </li>          
             <li> <b> Calculate Required alpha</b> it is not very usefull, just let it be there. </li>          
             </ul>
             <p> In all cases, you can set the required Type I error rate and whether the test will be carried out two-tailed or one-tailed.</b>

             </div>
             <p> In all cases, set the expected <b> R-squared </b> for the full model. 
             For models with only one independent variable the R-square is calculated
             as the square of the beta coefficients.<p>
             <p> Set the <b> Model degrees of freedom</b>. 
              If the model degrees of freedom are not easy to compute, please use the 
             <b> Model definition </b> panel to help you out. <p>
             </div>

  "
  return(text)

}

.text_intro.variance <- function(obj) {
  
  text<-" <div>
              <p> Please select the aim of the analysis:</p>
             <ul>
             <li> <b> Calculate N</b> computes the required sample size given the <b> Expected partial Eta-squared</b> and <b> Minimal desire power</b>  </li>          
             <li> <b> Calculate Power</b> computes the achievable power given the <b> Expected partial Eta-squared</b> and <b> N (Sample size)</b>  </li>          
             <li> <b> Calculate Effect Size</b> computes the minimally-detectable effect size given the <b> N (Sample size)</b> and <b> Minimal desire power</b>  </li>          
             <li> <b> Calculate Required alpha</b> it is not very usefull, just let it be there. </li>          
             </ul>
             <p> In all cases, you can set the required Type I error rate and whether the test will be carried out two-tailed or one-tailed.
             <p> If the expected Partial eta-squared is computed from data, it is recomended to use an adjusted version, such as
                 the <b>partial Omega-squared</b>, or the <b>Partial Epsilon-squared</b>. The <b> Option </b> panel may assist in their computation.<p/>
             

             </div>
             <p> Set the <b> Model degrees of freedom</b>. 
              If the model degrees of freedom are not easy to compute, please use the 
             <b> Model definition </b> panel to help you out. <p>
             </div>

  "
  return(text)

}



