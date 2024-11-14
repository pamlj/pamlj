## checkdata:       (required) this prepares all the info required to estimate the power parameters

.checkdata.beta <- function(obj) {
  
      obj$data<-data.frame(n = obj$options$n,
                       es = as.numeric(obj$options$b_es),
                       power=obj$options$power,
                       df_model = obj$options$b_df_model,
                       df_effect = 1,
                       sig.level = obj$options$sig.level)
      obj$data[[obj$aim]]<-NULL
      obj$info$letter      <- greek_vector["beta"]
      obj$info$esmax       <-  .999999
      obj$info$esmin       <- .001
      obj$info$loges       <-  function(x) x < .05
      obj$info$alternative <- obj$options$alternative
      obj$info$nmin        <- obj$data$df_model+2
      obj$info$logy        <- TRUE
      
      obj$info$ri2         <- 0
      obj$info$toaes            <- function(value) value^2*(1-obj$info$ri2)/(1-obj$info$r2)
      obj$info$fromaes          <- function(value) {
                                                   sqrt( value * (1-obj$info$r2)/ (1-obj$info$ri2) ) 
                               }

       if (is.something(obj$data$es)) {
        
               if (abs(obj$data$es) > obj$info$esmax) {
                    obj$stop("Beta coefficient absolute value cannot be larger than " %+% obj$info$esmax)
                 return()
               }
               
        }
      
      
      if (is.something(obj$options$rx)) {

           if (length(obj$options$rx)< 2) {
              obj$warning<-list(topic="powertab",message=paste("Variables in 'Correlations' do not define a squared correlation matrix. Correlations are ignored"))
              return()
           }
           
           local_df<-length(obj$options$rx)
           if (local_df >  obj$data$df_model) {
              obj$warning<-list(topic="powertab",message=paste("Model df cannot be less then the number of covariates. Model df are set to",local_df))
              obj$data$df_model<-local_df     
           }
           
           if (nrow(obj$analysis$data) > 0) {
                
                 rows<-max(2,length(obj$options$rx))
                 data<-subset(obj$analysis$data,select=obj$options$rx)
                 rtarget<-data[2:rows,obj$options$rx[1]]
                 rcovs<-data[2:rows,obj$options$rx[2:ncol(data)]]
                 bobj<-try_hard(chol2inv(as.matrix(rcovs))  %*% as.numeric(rtarget))
                 if (!isFALSE(bobj$error)) {
                    obj$stop("The correlation matrix cannot be used to compute the power. Please check the correlations")
                 }
                 else
                         beta <- bobj$obj
                 r2 <- sum(beta*rtarget)
                 if (r2 > 1 || r2 < 0) obj$stop("The correlation matrix yields impossible values for the R-squared. Please check the correlations.")
                 obj$info$ri2<-r2
            }  
      }
      
      if (is.something(obj$data$df_model)) {
                 if (obj$data$df_model < 1)
                           obj$stop("Model degrees of freedom cannot be less than 1")
                 if (obj$data$df_model == 1 && is.something(obj$data$es)) {
                           obj$warning<-list(topic="powertab",message="When df=1 the R-square is the square of the beta coefficient.")
                           obj$info$r2<-obj$data$es^2
                }
      } else {
                    stop("GLM power analysis based on beta coefficients requires the expected degrees of freedom of the model")
         }
      if (is.something(obj$info$r2 ) ) {

          if (abs(obj$info$r2)>.99)
                     obj$stop("The R-squared cannot be more than .99")
        
          if (is.something(obj$data$es))
             if ( obj$info$r2 < obj$data$es^2  )
                    obj$stop("R-squared cannot be less than the square of the beta coefficient")
   
        } else {
                obj$stop("GLM power analysis based on beta coefficients requires an expected R-squared for the model")
       }

}

.checkdata.peta <- function(obj) {

      
      obj$data<-data.frame(n = as.numeric(obj$options$n),
                       es = as.numeric(obj$options$v_es),
                       power=obj$options$power,
                       df_model = obj$options$v_df_model,
                       df_effect = obj$options$v_df_effect,
                       sig.level = obj$options$sig.level)
      obj$info$letter      <- letter_peta2
      obj$info$esmax       <-  .999999
      obj$info$esmin       <- 1e-08
      obj$info$eslbound    <- 0
      obj$info$loges            <-  function(x)  x < .05
      obj$info$alternative <- obj$options$alternative
      obj$info$nmin             <- obj$data$df_model+2
      obj$info$logy             <- TRUE
      obj$info$toaes            <- function(value) value/(1-value)
      obj$info$fromaes          <- function(value) value/(1+value)
      
      
  
    if (is.something(obj$data$es)) {
        
               if (obj$data$es > obj$info$esmax) {
                    obj$stop("Partial eta-squared coefficient cannot be larger than " %+% obj$info$esmax)
                 return()
               }
        }    

     
    if (obj$data$df_effect<1)
             obj$stop("Effect degrees of freedom cannot be less than 1")
      
    if (is.something(obj$data$df_model)) {
                if (obj$data$df_model < 1)
                           obj$stop("Model degrees of freedom cannot be less than 1")
    } else {
        obj$stop("GLM power analysis based on eta-squared requires the expected degrees of freedom of the model")
    }
  
  
    if ( obj$data$df_model < obj$data$df_effect ) {
           obj$data$df_model <- obj$data$df_effect
           obj$warning<-list(topic="issues",message="Model degrees of freedom cannot be less than the effect degrees of freedom. 
                                                   They have been set equal. ", head="info")
    }

     obj$data[[obj$aim]]<-NULL
  
}

.checkdata.eta <- function(obj) {

      obj$data<-data.frame(n = obj$options$n,
                       es = as.numeric(obj$options$e_es),
                       power=obj$options$power,
                       df_model = obj$options$e_df_model,
                       df_effect = obj$options$e_df_effect,
                       sig.level = obj$options$sig.level)
      obj$data[[obj$aim]]<-NULL
      obj$info$letter      <- letter_eta2
      obj$info$esmax       <-  .999999
      obj$info$esmin       <- .001
      obj$info$eslbound    <- 0
      obj$info$loges            <-  function(x) x < .05
      obj$info$alternative <- obj$options$alternative
      obj$info$nmin             <- obj$data$df_model+2
      obj$info$logy             <- TRUE
      obj$info$r2               <- obj$options$e_r2

      obj$info$toaes            <- function(value) {
                                                    peta <- value/(1-obj$info$r2+value)
                                                    peta/(1-peta)
                                                    }
      obj$info$fromaes          <- function(value)  {
                                                      peta<-value/(1+value)
                                                      peta*(1-obj$info$r2)/(1-peta)
                                                    }


  if (is.something(obj$data$es)) {
     if (abs(obj$data$es)<0)
         stop("Eta-squared value cannot be less than 0.")
     if (abs(obj$data$es)>.99)
         stop("Eta-squared value cannot be more than .99")
  }
  
  if (obj$data$df_effect<1)
             obj$stop("Effect degrees of freedom cannot be less than 1")
      
  if (is.something(obj$data$df_model)) {
                if (obj$data$df_model < 1)
                           stop("Model degrees of freedom cannot be less than 1")
    } else {
        stop("GLM power analysis based on eta-squared requires the expected degrees of freedom of the model")
    }
  
  
    if ( obj$data$df_model < obj$data$df_effect ) {
           obj$data$df_model <- obj$data$df_effect
           obj$warning<-list(topic="powertab",message="Model degrees of freedom cannot be less than the effect degrees of freedom. 
                                                   They have been set equal. ")
    }
      
    if ( obj$data$df_model == 1 && is.something(obj$data$es)) {
           obj$info$r2 <- obj$data$es
    #       obj$warning<-list(topic="powertab",message="With a model of 1 degree of freedom the R-squared must be equal to the Eta-squared. 
    #                                               The R-squared has been set equal to Eta-squared. ")
    }
  

    if (is.something(obj$info$r2 ) ) {
        if ( is.something(obj$data$es) && obj$info$r2+.0001 < obj$data$es  )
                   obj$stop("R-squared cannot be less than the eta coefficient")
      
        if (abs(obj$info$r2)>.99999)
                   obj$stop("The R-squared cannot be more than .99999")
    } else {
        obj$stop("GLM power analysis based on Eta-squared coefficient requires an expected R-squared for the model")
    }

}



## powervector:     (required) pass the data, with adjutment, to the lowerlevel power functions 

.powervector.glm <- function(obj,data) {

                aim<-required_param(data)
                if (is.something(data$es)) {
                                     data$f2<-obj$info$toaes(data$es)
                                     data$es<-NULL
                }
                else 
                    data$f2 <- NULL

                if (!is.something(data$n)) 
                                     data$v<-NULL
                else
                    data[["v"]]<- data$n - data$df_model -1

                 results<-lapply(1:nrow(data),function(i) {
                   one<-data[i,]
                   tryobj<-try_hard(pamlj.glm(u=one$df_effect,
                             v=one$v,
                             f2=one$f2,
                             power=one$power,
                              sig.level=one$sig.level,
                              df_model=one$df_model,
                              ncp_type=obj$options$ncp_type,
                              alternative=as.character(obj$info$alternative)
                              ), silent=T)
                   out<-tryobj$obj
                 
                   if (!isFALSE(tryobj$error)) {
                     out<-NULL
                     switch(aim,
                            n = {
                               ## if it fails, it means that the required N is smaller than 0, so we report the minimum N
                               n<-obj$info$nmin
                               v <- n- one$df_model -1
                               out<-list(u=one$df_effect,v=v,f2=one$f2,sig.level=one$sig.level,power=one$power,n=n,encp=0,method="nmin")
                               }
                            
                            )
                   }
                    out
                    })
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results)))  results[[i]]<-unlist(results[[i]])
                 odata<-data[, !names(data) %in% names(results)]
                 results<-cbind(odata,results)
                 results$es<-obj$info$fromaes(results$f2)
                 results$df1 <-results$df_effect
                 results$df2 <-ceiling(results$v)
                return(results)
}



## powertab_init:   (not required) this function produces or format the main table, powertab, before running
## NO NEED: default is used
## powertab:    (not required) this function produces or format the main table, powertab, after running
## NO NEED: default is used

## powerbyes:       this function produces or format the powerbyes table , after the estimation is done
## NO NEED: default is used

## powerbyen:       this function produces or format the powerbyen table , after the estimation is done
## NO NEED: default is used

## effectsize_init: (not required) some sub.module requires additional effect size to be computed. This function inits the table

.effectsize_init.beta <- function(obj) {

    return(list(
               list(index=letter_r2),
               list(index="Effect tollerance"),
               list(index=paste("Effect",letter_peta2)),
               list(index="Model df")
                 ))
}

.effectsize_init.eta <- function(obj) {

   tab <- list()
   ladd(tab)<-list(index=letter_r2,value=obj$info$r2)
   peta<-obj$data$f2/(1+obj$data$f2)
   if (length(peta)==0) peta<-"."
   ladd(tab)<-list(index=paste("Effect",letter_peta2),value=peta)
   ladd(tab)<-list(index="Model df",value=obj$data$df_model)

   return(tab)
  
}

## effectsize_run:  (not required) some sub.module requires additional effect size to be computed. This function fills the table

.effectsize_run.beta <- function(obj) {

   tab <- list()
   ladd(tab)<-list(value=obj$info$r2)
   tol<-1-obj$info$ri2
   ladd(tab)<-list(value=tol)
   peta<-obj$data$f2/(1+obj$data$f2)
   ladd(tab)<-list(value=peta)
   ladd(tab)<-list(value=obj$data$df_model)
   return(tab)
  
}

.effectsize_run.eta <- function(obj) {

   .effectsize_init.eta(obj)
  
}


## extrainfo:       (better if is there) this function add some extra info to be given to the user when the option "explain" is selected

.extrainfo.glm <- function(obj) {

  if (!obj$option("explain")) return()  
  
  if (obj$options$covs==0 && obj$options$factors==0) {
    
    terms<-ifelse(obj$data$df_model>1,"terms","term")
    text<-" <p> Power parameters are computed for a general linear model with " %+% obj$data$df_model %+%
          " degrees of freedom, for an effect size " %+% obj$info$letter %+% " = " %+% format5(obj$data$es) %+%
          " with " %+% obj$data$df_effect %+% " degrees of freedom " %+%
          " and type I error rate set to " %+% obj$data$sig.level %+%
          " The model is equivalent to a regression with " %+% obj$data$df_model %+% " " %+% terms %+% " or" %+%
          " an ANOVA with " %+% (obj$data$df_model+1) %+% " groups.</p>" %+%
          "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format5(obj$data[[obj$aim]]) %+% ".</p>"
            
    obj$warning<-list(topic="extrainfo",message=text,head="info")
    
  } else {
    
    what<-as.character(10*(obj$options$covs>0) + (obj$options$factors>0))
    analysis<-NULL
    reffects<-NULL
    feffects<-NULL
    meffects<-NULL
    if (obj$options$covs > 0 ) {
           analysis <-" a <b>regression</b> "
           rterms    <- obj$options$covs
           inter    <- obj$options$covs_order
           order    <- rterms
           switch(inter, 
                  main   = order<-1 ,
                  order2 = order<-2 ,
                  order3 = order<-3
                  )
          .effects<-sapply(1:order,function(x) ncol(combn(rterms,x)))
           reffects<-.effects[1] %+% " linear effect(s) "
           if (order > 1)
              reffects <- reffects %+% ", " %+% paste(.effects[2:length(.effects)],paste0(2:length(.effects),"-way"),"interaction(s)",collapse=", ")
    }
    
    if (obj$options$factors > 0) {
      
           analysis <-" an <b>ANOVA</b> "
           fterms    <- obj$options$factors
           inter    <- obj$options$factors_order
           levels  <- obj$options$factor_list
           order    <- fterms
           switch(inter, 
                  main   = order<-1 ,
                  order2 = order<-2 ,
                  order3 = order<-3
                  )
          .effects<-sapply(1:order,function(x) ncol(combn(fterms,x)))
           feffects<-.effects[1] %+% " main effects(s) "
           if (order > 1)
              feffects <- feffects %+% ", " %+% paste(.effects[2:length(.effects)],paste0(2:length(.effects),"-way"),"interaction(s)",collapse=", ")
           
      }
    
     if (obj$options$factors > 0 && obj$options$covs) {
            analysis <-" an <b>ANCOVA</b> "
            reffects<-reffects %+% " for continuous independent variables. "
            feffects<-feffects %+% " for categorical independent variables and "
            
          if (obj$options$mixed_order !="none") {
              switch(obj$options$mixed_order, 
                  main   = order<-1 ,
                  order2 = order<-2 ,
                  order3 = order<-3,
                  orderall= order<-rterms+fterms
                  )
             terms<-rterms+fterms
             .effects<-sapply(2:order,function(x) ncol(combn(terms,x)))
              meffects <- "The model incluses also " %+% 
                           paste(.effects[2:length(.effects)],paste0(2:length(.effects),"-way"),"interaction(s)",collapse=", ") %+%
                          " between factors and covariates."
             
             
           }
     } 
    
      text<-" <p> Power parameters are computed for a general linear model with " %+% obj$data$df_model %+%
          " degrees of freedom, for an effect size " %+% obj$info$letter %+% " = " %+% format5(obj$data$es) %+%
          " with " %+% obj$data$df_effect %+% " degrees of freedom " %+%
          " and type I error rate set to " %+% obj$data$sig.level %+%
          " The model is equivalent to " %+% analysis %+% " with  terms: " %+%
            ifelse(is.null(feffects),"",feffects) %+%
            ifelse(is.null(reffects),"",reffects) %+%
            ifelse(is.null(meffects),"",meffects) %+%
          "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format5(obj$data[[obj$aim]]) %+% ".</p>"
      obj$warning<-list(topic="extrainfo",message=text,head="info")
      

  }
  

  
}
