## here are functions that are always called but return different information
## depending to the analysis being carried out


checkdata <- function(obj, ...) UseMethod(".checkdata")

# DC
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
      obj$info$esmin       <- .01

      if (obj$options$is_equi ) {
        
        if (abs(obj$options$equi_limit)>0) {
        obj$info$equi_limit <- obj$options$equi_limit
        obj$info$toaes<- function(value) abs(value-obj$info$equi_limit)
        obj$info$fromaes<- function(value) abs(value-obj$info$equi_limit)
        obj$info$esmin       <-  0
        obj$info$esmax       <-  obj$data$equi_limit-.0001
        }
        if (obj$options$equi_limit<obj$data$es) 
          stop("Equivalence tests require the expected effect size to be smaller than the equivalence limit")
         if (obj$options$equi_limit==0) 
          stop("Equivalence tests require the equivalence limit to be larger than 0")
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
          stop("Equivalence tests require the expected effect size to be smaller than the equivalence limit")
         if (obj$options$equi_limit==0) 
          stop("Equivalence tests require the equivalence limit to be larger than 0")

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
          stop("Equivalence tests require the expected effect size to be smaller than the equivalence limit")
         if (obj$options$equi_limit==0) 
          stop("Equivalence tests require the equivalence limit to be larger than 0")

      }


      
}

### correlation #### DC

.checkdata.correlation <- function(obj) {

     if (obj$options$es < 0)
                    obj$warning<-list(topic="issues",
                                     message="Negative correlations have the same power parameters of positive correlations. The absolute value is considered here."
                                     )
      obj$data<-data.frame(es=abs(obj$options$es))
      obj$data$n           <- obj$options$n
      obj$data$sig.level   <- obj$options$sig.level
      obj$data$power       <- obj$options$power
      obj$data$alternative <- obj$options$alternative
      obj$data[[obj$aim]]  <- NULL
      
      obj$info$letter      <- greek_vector["rho"]
      obj$info$esmax       <- .99
      obj$info$esmin       <- .01
      obj$info$nmin        <-  4


}

### proportions ###
.checkdata.propind <- function(obj) {

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

      obj$data<-data.frame(power=obj$options$power)
      obj$data$n          <- obj$options$propone_n
      obj$data$p1          <- obj$options$propone_p1
      obj$data$p2          <- obj$options$propone_p2
      .common_proportions(obj)

}

.checkdata.proppaired <- function(obj) {

      obj$data<-data.frame(power=obj$options$power)
      obj$data$n           <- obj$options$proppaired_n
      obj$data$alternative <- obj$options$alternative
      obj$data$sig.level   <- obj$options$sig.level
      
      obj$data$p1          <- obj$options$proppaired_p1
      obj$data$p2          <- obj$options$proppaired_p2
      
      if (obj$aim == "es") obj$data$p2<-1

      if (!is.something(obj$data$p1)) 
           stop("P21  is required")

      if (obj$data$p1 >= obj$data$p2)
           stop("P21  should be smaller than P12")

      switch(obj$options$es_type,
             dif = {
                   obj$data$es          <- obj$data$p2-obj$data$p1
               
                   obj$info$letter      <- greek_vector["Delta"] 
                   obj$info$esmax       <-  .5-obj$data$p1
                   obj$info$esmin       <-  .01
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
                   obj$info$esmin       <-  1.01
                   obj$info$loges            <-  function(x) x > 10
                   obj$info$toaes            <- function(data) {
                                               p1 <- data$p2/data$es
                                               data$p2/p1
                                              }
                   obj$info$fromaes            <- function(data)  (data$psi)
                                              
             }
             

      )
  
      
      obj$data[[obj$aim]]<-NULL
}


.common_proportions<-function(obj) {

        obj$data$sig.level   <- obj$options$sig.level
        obj$data$alternative <- obj$options$alternative
        
        obj$info$nmin         <- 6
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


### GLM ###


.checkdata.beta <- function(obj) {
  
      obj$data<-data.frame(n = obj$options$n,
                       es = as.numeric(obj$options$b_es),
                       power=obj$options$power,
                       df_model = obj$options$b_df_model,
                       df_effect = 1,
                       sig.level = obj$options$sig.level)
      obj$data[[obj$aim]]<-NULL
      obj$info$letter      <- greek_vector["beta"]
      obj$info$esmax       <-  .99
      obj$info$esmin       <- .01
      obj$info$loges            <-  function(x) x < .3
      obj$info$alternative <- obj$options$alternative
      obj$info$nmin             <- obj$data$df_model+2
      obj$info$logy             <- TRUE
      obj$info$r2          <- as.numeric(obj$options$b_r2)
      
      obj$info$ri2         <- 0
      obj$info$toaes            <- function(value) value^2*(1-obj$info$ri2)/(1-obj$info$r2)
      obj$info$fromaes          <- function(value) sqrt( value * (1-obj$info$r2)/ (1-obj$info$ri2) ) 


       if (is.something(obj$data$es)) {
               if (abs(obj$data$es)<.001) {
                    obj$stop("Beta coefficient absolute value cannot be less than .001")
                    return()
               }
               
               if (abs(obj$data$es)>.99) {
                    obj$stop("Beta coefficient absolute value cannot be larger than .99")
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
                           stop("Model degrees of freedom cannot be less than 1")
                 if (obj$data$df_model == 1 && is.something(obj$data$es)) {
                           obj$warning<-list(topic="powertab",message=paste("When df=1 the R-square is the square of the beta coefficient."))
                           obj$info$r2<-obj$data$es^2
                }
      } else {
                    stop("GLM power analysis based on beta coefficients requires the expected degrees of freedom of the model")
         }
      if (is.something(obj$info$r2 ) ) {

          if (abs(obj$info$r2)>.99)
                     stop("The R-squared cannot be more than .99")
        
          if (is.something(obj$data$es))
             if ( obj$info$r2+.0001 < obj$data$es^2  )
                    stop("R-squared cannot be less than the square of the beta coefficient")
   
        } else {
                stop("GLM power analysis based on beta coefficients requires an expected R-squared for the model")
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
      obj$info$esmax       <-  .99
      obj$info$esmin       <- .001
      obj$info$loges            <-  function(x) x < .3
      obj$info$alternative <- obj$options$alternative
      obj$info$nmin             <- obj$data$df_model+2
      obj$info$logy             <- TRUE
      obj$info$toaes            <- function(value) value/(1-value)
      obj$info$fromaes          <- function(value) value/(1+value)  

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
      obj$info$esmax       <-  .99
      obj$info$esmin       <- .001
      obj$info$loges            <-  function(x) x < .3
      obj$info$alternative <- obj$options$alternative
      obj$info$nmin             <- obj$data$df_model+2
      obj$info$logy             <- TRUE
      obj$info$r2               <- obj$options$e_r2

      obj$info$toaes            <- function(value) value/(1-obj$info$r2)
      obj$info$fromaes          <- function(value) value*(1-obj$info$r2) 

  
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
  
  
    if ( obj$data$df_model < obj$data$df_effect ) {
           obj$data$df_model <- obj$data$df_effect
           obj$warning<-list(topic="powertab",message="Model degrees of freedom cannot be less than the effect degrees of freedom. 
                                                   They have been set equal. ")
    }
      
    if ( obj$data$df_model == 1 &&  obj$data$es != obj$info$r2) {
           obj$info$r2 <- obj$data$es
           obj$warning<-list(topic="powertab",message="With a model of 1 degree of freedom the R-squared must be equal to the Eta-squared. 
                                                   The R-squared has been set equal to Eta-squared. ")
    }
  

    if (is.something(obj$info$r2 ) ) {
        if ( is.something(obj$data$es) && obj$info$r2+.0001 < obj$data$es  )
                   stop("R-squared cannot be less than the Eta coefficient")
        if (abs(obj$info$r2)>.99)
                   stop("The R-squared cannot be more than .99")
    } else {
        stop("GLM power analysis based on Eta-squared coefficient requires an expected R-squared for the model")
    }

}


.checkdata.facmeans <- function(obj) {
  
      jinfo("PAMLj: Checkdata factorial facmeans")

      obj$ok <- FALSE

      obj$info$letter      <- letter_peta2
      obj$info$esmax       <- .98
      obj$info$esmin       <- .001
      obj$info$alternative <- "two.sided"
      obj$info$r           <- obj$options$r 
      obj$info$toaes       <- function(value) value/(1-value) 
      obj$info$fromaes     <- function(value) value/(1+value)  
      means   <- obj$options$means
      sds     <- obj$options$sds
      factors <- obj$options$factors
      obj$data<-data.frame(power=obj$options$power,
                           sig.level=obj$options$sig.level)
      
      if (is.null(means))   return()
      if (is.null(sds))     return()
      if (is.null(factors)) return()

      exdata  <-obj$analysis$data
      if (nrow(stats::na.omit(exdata)) != nrow(obj$analysis$data))
        stop("Dataset cannot contain missing values")
      within  <- unlist(obj$options$within)
      between <- setdiff(factors,within)

      if (!is.something(within))       obj$info$r <- 0 
        
      if (nrow(exdata) > 0) {
                nlevbet<-1
                nlevwit<-1
                for (f in factors) {
                        exdata[[f]]<-factor(exdata[[f]])
                        contrasts(exdata[[f]])<-contr.sum(nlevels(exdata[[f]]))
                        if (f %in% between) nlevbet<-nlevbet*nlevels(exdata[[f]])
                        else nlevwit<-nlevwit*nlevels(exdata[[f]])
                 }
         
        ## we need to be sure that jamovi does not pass the data as factors
        ## like when the sd are a computed variable with one integer
                exdata[[means]]<-as.numeric(as.character(exdata[[means]]))
                exdata[[sds]]<-as.numeric(as.character(exdata[[sds]]))
                
                # now we start computing the SS
                form<-paste(means,"~",paste(factors,collapse="*"))
                aa<-stats::aov(as.formula(form),data=exdata)
                sumr<-summary(aa)[[1]]
                res<-as.data.frame(sumr)
                res$source<-trimws(rownames(res))
                res<-res[res$source!="Residuals",]
                res$type<-unlist(lapply(res$source, function(x) {
                            terms<-stringr::str_split(x,":",simplify=T)
                            terms<-trimws(terms)
                            test <-length(intersect(terms,within))>0
                            if (test) return("w")
                            else return("b")
                           }))
            res$edfw<-0
            res$edfb<-0
            res$cell<-0
            for (i in seq_len(nrow(res))) {
             type<-res$type[i]
             x<-res$source[i]
             terms<-stringr::str_split(x,":",simplify=T)
             terms<-trimws(terms)
             wits <-intersect(terms,within)
             val<-1
             for (f in wits) val<-val*(nlevels(exdata[[f]])-1)
             res$edfw[i]<-val
             bets <-intersect(terms,between)
             val<-1
             for (f in bets) val<-val*(nlevels(exdata[[f]])-1)
             res$cell[i]<-val
             res$edfb[i]<-nlevbet-1
 
            }
            
        ### this formulas are equivalent to standard computation of SS for mixed anova
        ### they are slightly different in order to reflect the computation 
        ### of SS in car::Anova() dividing everything by the error DF.
        ### They lead to the correct partial eta-square anyway.

        for (i in seq_len(nrow(res))) {
          if (res$type[i]=="b") res$ss[i]<-res$`Sum Sq`[i]/nlevbet
          else               res$ss[i]<-res$cell[i]*res$`Sum Sq`[i]/(nlevbet*res$Df[i])
        }
                 form<-paste(sds,"~",paste(factors,collapse="*"))
                 aa<-stats::aov(as.formula(form),data=exdata)
                 msds<-as.data.frame(emmeans::emmeans(aa,specs=factors))

                 mse<-mean(msds$emmean^2)
                 for (i in 1:nrow(res)) {
                        if (res$type[i]=="w") {
                               res$sigma2[i] <- mse*(1-obj$info$r)
                               
                        } else {
                               res$sigma2[i] <- mse*(1+(nlevwit-1)*obj$info$r)
                        }
                  } 

                 res$es<-res$ss/(res$ss+res$sigma2)
                 res$n=obj$options$n
                 res$sig.level=obj$options$sig.level
                 res$power=obj$options$power
                 res$df_effect<-res$Df
                 res$df_model<- sum(res$df_effect)
                 obj$extradata<-res
                 obj$extradata[[obj$aim]]<-NULL
                 obj$extradata$id<-1:nrow(obj$extradata)
                 pwr<-powervector(obj,obj$extradata)
         ## we select the effect to focus on
                 if (obj$aim=="n") 
                     w<-which.max(pwr$n)
                 else
                     w<-which.min(pwr$es)
                 w<-w[1]
                 obj$data <- subset( obj$extradata, obj$extradata$id==w)
                 obj$data[[obj$aim]]<-NULL
                 obj$info$nmin <- obj$data$df_model + 10  
        # at least one parameter should be empty for parameters estimation
                 obj$ok <- TRUE
                 } else {
                  form<-as.formula(paste(means,"~",paste(factors,collapse="*")))
                .names<-attr(terms(form),"term.labels")
                 obj$data<-data.frame(source=.names, 
                              power=obj$options$power,
                              sig.level=obj$options$sig.level)
     
                 }         

}


.checkdata.facpeta <- function(obj) {
  
      jinfo("PAMLj: Checkdata factorial facpeta")
      obj$info$letter      <- letter_peta2
      obj$info$esmax       <- .99
      obj$info$esmin       <- .001
      obj$info$alternative <- "two.sided"
      obj$info$toaes       <- function(value) value/(1-value) 
      obj$info$fromaes     <- function(value) value/(1+value)  
      obj$data<-data.frame(power=obj$options$power,
                           sig.level=obj$options$sig.level,
                           n=obj$options$n)
      
      obj$data$es<-obj$options$peta
      design<-obj$options$effect_type
      type<-obj$options$repeated_type

      if (design=="within") {
         obj$data$type<-"w"
         obj$data$edfb <- obj$options$design_groups-1
         #if (obj$data$edfb==0) obj$data$edfb<-1
         obj$data$edfw <- obj$options$df_effect
         obj$data$df_effect<-obj$data$edfw
         obj$data$df_model<- obj$options$design_groups-1
         obj$data$source<-"Within"
         
      } else {
         obj$data$type<-"b"
         obj$data$edfb <- obj$options$design_groups-1
         obj$data$edfw <- 1
         obj$data$source<-"Between"
         obj$data$df_model<- obj$options$design_groups-1
         obj$data$df_effect<- obj$options$df_effect

      }
   
      
      obj$data[[obj$aim]]<-NULL
      
}





checkfailure <- function(obj, ...) UseMethod(".checkfailure")

.checkfailure.default <- function(obj,results) {

  what <- obj$aim
  nice <- nicify_param(what)
  test<-grep("values at end points not of opposite", results) 
  if (length(test) > 0) {
    message <- paste(nice, " cannot be computed for the combination of input parameters")
    switch (what,
      n = message <- paste(message,"The require power may be too low for or the effect size is not a reasonable value")
    )
   obj$warning<-list(topic="issues",message=message,head="error")
   obj$ok <- FALSE # no good to go further  
  }
  
}


.checkfailure.proppaired <- function(obj,results) {
 
  what <- required_param(obj$input)
  
  if (what == "es") {
    
    msg<-"A suitable (meaningfull) effect size cannot be found for the given P12 and N"
    data    <-obj$data
    p2      <-1-data$p1
    data$es <-(p2/data$p1)-.0001
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


### additional check for models



commonchecks <- function(obj) {
  
    if (is.something(obj$data$sig.level ) ) {
        if ( any(obj$data$sig.level < 0.00001) ||  any(obj$data$sig.level > .90)) {
                   obj$warning<-list(topic="issues",message=paste("Type I error rate should be between .00001 and .90"),head="error")
                   obj$ok<-FALSE
        }
    } else {
        stop("Power analysis requires Type I rate")
    }
  
    if (is.something(obj$data$power ) ) {
        if ( any(obj$data$power < 0.10) ||  any(obj$data$power > .99)) {
                   obj$warning<-list(topic="issues",message=paste("Power should be between .10 and .99"),head="error")
                   obj$ok<-FALSE
        }
        if ( any(obj$data$power < obj$data$sig.level) ) {
                   obj$warning<-list(topic="issues",message=paste("Power should be larger than Type I error rate "),head="error")
                   obj$ok<-FALSE
        }

    } 

    if (is.something(obj$data$n ) ) {
        if ( any(obj$data$n < obj$info$nmin )) {
                   obj$stop(paste("N (total sample size) should be larger than",obj$info$nmin))
        }
    } 
    if (is.something(obj$data$es ) ) {
        if ( any(obj$data$es < obj$info$esmin )) {
                   obj$stop(paste0("The effect size ",obj$info$letter," should be larger than",obj$info$esmin))
        }
    } 
  
 

}

morechecks <- function(obj, ...) UseMethod(".morechecks")

.morechecks.default<-function(obj) {
    jinfo("PAMLj: more checks default")
  
    data<-obj$data
    if (obj$aim == "n") {
      data$n <- obj$info$nmin
      esmax  <- round(find_max_es(obj,data), digits=3)
      es     <- round(data$es,digits=3) 
      if (data$es > esmax) {
                   message<-paste0("The effect size (",obj$info$letter," = ", es,") is larger than the maximum effect size (",obj$info$letter,"=",esmax,")",
                                   " that guarantees power=",data$power," with a sample of minimum size (N=",data$n,").",
                                   "This means that any effect size larger than ", esmax ," guarantees a power > ",data$power," for any sample size equal or larger than ",data$n,".")
                   obj$warning<-list(topic="issues",message=message,head="info")
      }
      esmin<-round(find_min_es(obj,data), digits=3)
      if (data$es < esmin) {
                   message<-paste0("The effect size (",obj$info$letter," = ",es,") is smaller than the minimum effect size (",obj$info$letter,"=",esmin,")",
                                   " requiring a huge a sample size (N=",obj$info$nmax,") for power=",data$power,". ",
                                   "This means that any effect size smaller than ", esmin ," needs a sample larger than ",obj$info$nmax_spell," to guarantees a power = ",data$power,".")
                   obj$warning<-list(topic="issues",message=message,head="info")
    }
  
    }

}


.morechecks.proportions<-function(obj) {
  
  jinfo("PAMLj: more checks for proportions")
 
  if (utils::hasName(obj$data,"aprox")) {
    switch (obj$data$aprox,
      nmin = {     message<-paste0("The effect size (",obj$info$letter," = ",es,") is too large to compute the required N. The minimum sample size is reported instead.",
                                   "This means that a sample size of ",data$info$nmin," guarantees a power equal or larger than ", obj$data$power)
                   obj$warning<-list(topic="issues",message=message,head="info")}
    )
  }
}


postchecks <- function(obj, ...) UseMethod(".postchecks")

.postchecks.default<-function(obj) {
  
  morechecks(obj)
  jinfo("PAMLj: post checks default")
  if (utils::hasName(obj$data,"aprox")) {
    switch (obj$data$aprox,
      nsmall = {     message<-paste0("The effect size (",obj$info$letter," = ",round(obj$data$es,digits=3),") is so large that the computed required N is smaller than a practical sample size. The minimum sample size is reported instead.",
                                   "This means that a sample size of ",obj$info$nmin," guarantees a power equal or larger than ", obj$data$power," for the input effect size.")
                   obj$warning<-list(topic="issues",message=message,head="warning")},
      nmin = {   message<-paste0("The effect size (",obj$info$letter," = ",round(obj$data$es,digits=3),") is too large to compute the required N. The minimum sample size is reported instead.",
                                   "This means that a sample size of ",obj$info$nmin," guarantees a power equal or larger than ", obj$data$power," for the input effect size.")
                   obj$warning<-list(topic="issues",message=message,head="warning")}
   
    )
  }
  
  if (obj$data$power>.9999) {
    
                   message<-paste0("The analysis reports a power close to 1.  Sensitivity analyses may be not accurate.")
                   obj$warning<-list(topic="plotnotes",message=message,head="warning")
    
  }
  if (obj$data$n < obj$info$nsave) {
    
                   message<-paste0("The analysis yields a very small sample size.  Sensitivity analyses may be not accurate.")
                   obj$warning<-list(topic="plotnotes",message=message,head="warning")
    
  }


}

