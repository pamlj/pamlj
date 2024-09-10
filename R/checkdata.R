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

### correlation #### DC

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

### proportions ###
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
      obj$info$esmax       <-  .999999
      obj$info$esmin       <- .001
      obj$info$loges       <-  function(x) x < .3
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
                           stop("Model degrees of freedom cannot be less than 1")
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
      obj$info$loges            <-  function(x) x < .3
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
      obj$info$loges            <-  function(x) x < .3
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


.checkdata.facmeans <- function(obj) {
  
      jinfo("PAMLj: Checkdata factorial facmeans")

      obj$ok <- FALSE

      obj$info$letter      <- letter_peta2
      obj$info$esmax       <- .999999
      obj$info$esmin       <- .001
      obj$info$eslbound    <- 0
      obj$info$alternative <- "two.sided"
      obj$info$r           <- obj$options$r 
      obj$info$toaes       <- function(value) value/(1-value) 
      obj$info$fromaes     <- function(value) value/(1+value)  
      means   <- obj$options$means
      sds     <- obj$options$sds
      factors <- obj$options$factors
      ## derive terms
      obj$data<-data.frame(power=obj$options$power,
                           sig.level=obj$options$sig.level)
      
      needed <-c("Factors","Means","Standard deviations")
      needed <- needed[c(is.null(factors),is.null(means),is.null(sds))]
      if (length(needed)) {
            text <- "<p>Please fill in the required input:</p> <ul>" 
            for (ned in needed) text <- text %+% "<li>" %+% ned %+% "</li>" 
            text <-  text %+% "</ul>"
            obj$warning<-list(topic="issues",message=text,head="info")
      }
      if (is.null(means))   return()
      if (is.null(sds))     return()
      if (is.null(factors)) return()

      exdata  <-obj$analysis$data
      
      form<-paste("means~",paste(factors,collapse="*"))
      obj$info$terms<-attr(terms(as.formula(form)),"term.labels")

      
      if (nrow(stats::na.omit(exdata)) != nrow(obj$analysis$data))
        obj$stop("Dataset cannot contain missing values")
      
      obj$extradata<-exdata
      
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
                 if (length(obj$info$terms)>1)
                       obj$warning<-list(topic="powerbyes",message="Sensitivity analysis is done on the smallest effect (" %+% obj$info$terms[w] %+% ")")
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
      obj$info$esmax       <- .999999
      obj$info$esmin       <- .001
      obj$info$eslbound    <- 0
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



### mediation #### 

.checkdata.medsimple <- function(obj) {

      jinfo("Checking data for medsimple")

  

      obj$data             <- data.frame(b=obj$options$b)
      obj$data$a           <- obj$options$a
      betas<-list(a=obj$data$a,b=obj$data$b,"c"=obj$data$cprime)
      test <- check_parameters(betas, fun=function(x) (!is.null(x) && abs(x)<.001), verbose=FALSE)
      if (length(test)>0) obj$stop("Standardized coefficients (absolute value) cannot be smaller than .001. Please correct coefficients: " %+% paste(test,collapse=", "))
      
      obj$data$cprime      <- obj$options$cprime
      betas$cprime <- obj$data$cprime
      test <- check_parameters(betas, fun=function(x) (!is.null(x) && abs(x) >.99), verbose=FALSE)
      if (length(test)>0) obj$stop("Standardized coefficients cannot be larger than .99. Please correct coefficients: " %+% paste(test,collapse=", "))

      if (is.something(obj$data$a)) {
                      obj$data$es<-obj$data$a*obj$data$b
                      obj$info$rxy<-obj$data$a*obj$data$b+obj$data$cprime
      }
      
      obj$data$n           <- obj$options$n
      obj$data$sig.level   <- obj$options$sig.level
      obj$data$power       <- obj$options$power
      obj$data$alternative <- obj$options$alternative
      obj$data$test        <- obj$options$test
      obj$data$R           <- obj$options$mcR
      obj$data$parallel    <- obj$options$parallel

      obj$plots$data       <- obj$data

      obj$data[[obj$aim]]  <- NULL
      obj$info$letter      <- "ME"
      obj$info$esmax       <- .9801
      obj$info$esmin       <-  1e-06
      obj$info$nmin        <-  10
      obj$info$nochecks    <-  "es"
      jinfo("Checking data for medsimple done")
}


.checkdata.medcomplex <- function(obj) {
  
       jinfo("Checking data for medcomplex")

        if (obj$aim == "es") obj$stop("Finding minumum effect size for complex mediation model is not implemented yes")
  
        bs<-list(a1=obj$options$a1,b1=obj$options$b1,a2=obj$options$a2,b2=obj$options$b2,a3=obj$options$a3,b3=obj$options$b3)
        numbs<-sapply(bs,as.numeric)
        ### some checks      
        
        test <- check_parameters(numbs, fun=function(x) (!is.na(x) && abs(x) >.99), verbose=FALSE)
        if (length(test)>0) obj$stop("Standardized coefficients cannot be larger than .99. Please correct coefficients: " %+% paste(test,collapse=", "))
        test <- check_parameters(numbs, fun=function(x) (!is.na(x) && abs(x)<.001), verbose=FALSE)
        if (length(test)>0) obj$stop("Standardized coefficients (absolute value) cannot be smaller than .001. Please correct coefficients: " %+% paste(test,collapse=", "))

        
        #### here we go
        switch (obj$options$model_type,
              twomeds = {
                        betas           <- bs[1:4]
                        betas$r12       <- obj$options$r12
                        check           <- lapply(betas, as.numeric,USE.NAMES=T)
                        plotdata        <- data.frame(do.call(cbind,check))
                        plotdata$cprime <- obj$options$cprime2
                        obj$plots$data  <- plotdata
                     
                        if (any(sapply(check, is.na))) obj$filled<-FALSE
                        
                        exdata             <- data.frame(id=1:2)
                       
                        if (obj$filled) {
      
                          exdata$cprime <- plotdata$cprime
                          exdata$a      <- c(plotdata$a1,plotdata$a2)
                          exdata$b      <- c(plotdata$b1,plotdata$b2)
                          corMat      <- diag(4)
                          corMat[2,1] <- corMat[1,2] <- plotdata$a1
                          corMat[3,1] <- corMat[1,3] <- plotdata$a2
                          corMat[2,3] <- corMat[2,3] <- plotdata$r12
                          corMat[4,1] <- corMat[1,4] <- plotdata$cprime + plotdata$a1*plotdata$b1 + plotdata$a2*plotdata$b2
                          corMat[2,4] <- corMat[4,2] <- plotdata$a1*plotdata$cprime + plotdata$b1 + plotdata$b2*plotdata$r12
                          corMat[3,4] <- corMat[4,3] <- plotdata$a2*plotdata$cprime + plotdata$b2 + plotdata$b1*plotdata$r12
                          
                          ry<-corMat[1:3,4]
                          rx<-corMat[1:3,1:3]  
                          
                          r2b <- t(ry)%*%MASS::ginv(rx)%*%ry
                          
                          if (r2b > .99) {
                                        obj$stop("Input coefficients are not feasable. The resulting R-squared are impossible. Please adjust the input values")
                          }

                          obj$info$rxy<-corMat[4,1]
                          exdata$r2y <- as.numeric(r2b)
                          exdata$r2a <- exdata$a^2
                          exdata$es  <-  exdata$a * exdata$b
                          exdata$effect <- c("a1*b1","a2*b2")

                        } else {
                          
                          needed <-c("X to M1 (a1)","M1 to Y (b1)","X to M2 (a2)","M2 to Y (b2)","M1-M2 correlation (r12)")
                          names(check)<-needed
                          test<-check_parameters(check, fun=is.na)
                          if (length(test)>0) obj$warning<-list(topic="issues",message=test,head="info")

                          
                        }
                        
                        },
              threemeds = {
                        betas <- bs
                        betas$r12 <- obj$options$r12
                        betas$r13 <- obj$options$r13
                        betas$r23 <- obj$options$r23
                        check <- lapply(betas, as.numeric,USE.NAMES=T)
                       
                        plotdata<- data.frame(do.call(cbind,check))
                        plotdata$cprime      <- obj$options$cprime2
                        obj$plots$data       <- plotdata

                        if (any(sapply(check, is.na))) obj$filled<-FALSE
                        
                        exdata        <- data.frame(id=1:3)

                        if (obj$filled) {
                          exdata$cprime <- plotdata$cprime
                          exdata$a      <- c(plotdata$a1,plotdata$a2,plotdata$a3)
                          exdata$b      <- c(plotdata$b1,plotdata$b2,plotdata$b3)
                          corMat <- diag(5)
                          corMat[2,1] <- corMat[1,2] <- plotdata$a1
                          corMat[3,1] <- corMat[1,3] <- plotdata$a2
                          corMat[4,1] <- corMat[1,4] <- plotdata$a3
                          corMat[2,3] <- corMat[3,2] <- plotdata$r12
                          corMat[2,4] <- corMat[4,2] <- plotdata$r13
                          corMat[3,4] <- corMat[4,3] <- plotdata$r23
  
                          corMat[5,1] <- corMat[1,5] <- plotdata$cprime + plotdata$a1*plotdata$b1 + plotdata$a2*plotdata$b2 + plotdata$a3*plotdata$b3
                          corMat[2,5] <- corMat[5,2] <- plotdata$a1*plotdata$cprime + plotdata$b1 + plotdata$b2*plotdata$r12 + plotdata$b3*plotdata$r13
                          corMat[3,5] <- corMat[5,3] <- plotdata$a2*plotdata$cprime + plotdata$b2 + plotdata$b1*plotdata$r12 + plotdata$b3*plotdata$r13
                          corMat[4,5] <- corMat[5,4] <- plotdata$a3*plotdata$cprime + plotdata$b3 + plotdata$b2*plotdata$r12 + plotdata$b1*plotdata$r13
  
                          colnames(corMat)<-rownames(corMat)<-c("X","M1","M2","M3","Y")
                     
                          ry <- corMat[1:4,5]
                          rx <- corMat[1:4,1:4]  
                          r2b <- t(ry)%*%MASS::ginv(rx)%*%ry
                          if (r2b > .99) {
                                        obj$stop("Input coefficients are not feasable. The resulting R-squared are impossible. Please adjust the input values")
                          }

                          exdata$r2y <- as.numeric(r2b)
                          exdata$r2a <- exdata$a^2
                          exdata$es  <-  exdata$a * exdata$b
                          obj$info$rxy<-corMat[5,1]
                          exdata$effect <- c("a1*b1","a2*b2","a3*b3")
                        } else {
                          
                          needed <-c("X to M1 (a1)","M1 to Y (b1)","X to M2 (a2)","M2 to Y (b2)", "X to M3 (a3)",
                                     "M3 to Y (b3)","M1-M2 Correlation (r12)","M1-M3 Correlation (r13)", "M2-M3 Correlation (r23)")
                          names(check)<-needed
                          test<-check_parameters(check, fun=is.na)
                          if (length(test)>0) obj$warning<-list(topic="issues",message=test,head="info")
                        }
                        
                        
                        },
              twoserial = {
                        betas <- bs[1:4]
                        betas$d1  <- obj$options$d1
                        check <- lapply(betas, as.numeric,USE.NAMES=T)
                        if (any(sapply(check, is.na))) obj$filled<-FALSE
                        plotdata<- data.frame(do.call(cbind,check))
                        plotdata$cprime      <- obj$options$cprime2
                        obj$plots$data       <- plotdata
                        exdata        <- data.frame(id=1:3)
                        obj$info$keffects <- 3 
                       
                        if (obj$filled) {
                          exdata$cprime <- plotdata$cprime
                          exdata$a      <- c(plotdata$a1,plotdata$a2,plotdata$a1)
                          exdata$b      <- c(plotdata$b1,plotdata$b2,plotdata$b2)
                          exdata$d1     <- c(NA,NA,plotdata$d1)

                         corMat <- diag(4)
                         corMat[2,1] <- corMat[1,2] <- plotdata$a1
                         corMat[3,1] <- corMat[1,3] <- plotdata$a2 + plotdata$d1*plotdata$a1
                         corMat[2,3] <- corMat[3,2] <- plotdata$d1 + plotdata$a1*plotdata$a2

                         corMat[4,1] <- corMat[1,4] <- plotdata$cprime + plotdata$a1*plotdata$b1 + plotdata$a1*plotdata$b2*plotdata$d1 + plotdata$a2*plotdata$b2
                         corMat[2,4] <- corMat[4,2] <- plotdata$a1*plotdata$cprime + plotdata$b1 + plotdata$b2*plotdata$d1 + plotdata$a1*plotdata$a2*plotdata$b2
                         corMat[3,4] <- corMat[3,4] <- plotdata$a2*plotdata$cprime + plotdata$b2 + plotdata$b1*plotdata$d1 + plotdata$a1*plotdata$cprime*plotdata$d1
                         
                         ry<-corMat[1:3,4]
                         rx<-corMat[1:3,1:3]  
                         r2b<-as.numeric(t(ry)%*%MASS::ginv(rx)%*%ry)
                         
                          if (r2b > .99) {
                                        obj$stop("Input coefficients are not feasable. The resulting R-squared are impossible. Please adjust the input values")
                          }

                         r2a<-plotdata$a1^2

                         ry<-corMat[c(1,2),3]
                         rx<-corMat[c(1,2),c(1,2)]
                         r2d1<-as.numeric(t(ry)%*%MASS::ginv(rx)%*%ry)
                        
                         exdata$es   <- 0
                         exdata$r2a  <- r2a
                         exdata$r2y  <- r2b
                         exdata$r2d1 <- c(NA,NA,r2d1) 

                         exdata$es[1] <- plotdata$a1*plotdata$b1
                         exdata$es[2] <- plotdata$a2*plotdata$b2
                         exdata$es[3] <- plotdata$a1*plotdata$d1*plotdata$b2
                         obj$info$rxy<-corMat[4,1]

                         exdata$effect <- c("a1*b1","a2*b2","a1*d1*b2")
 

                        } else {
                          
                          needed <-c("X to M1 (a1)","M1 to Y (b1)","X to M2 (a2)","M2 to Y (b2)","M1 to M2 (d1)")
                          names(check)<-needed
                          test<-check_parameters(check, fun=is.na)
                          if (length(test)>0) obj$warning<-list(topic="issues",message=test,head="info")
                        }
                    }
              
      )

      obj$extradata<- exdata
      obj$extradata$n           <- obj$options$n
      obj$extradata$sig.level   <- obj$options$sig.level
      obj$extradata$power       <- obj$options$power
      obj$extradata$alternative <- obj$options$alternative
      obj$extradata$test        <- obj$options$test
      obj$extradata$R           <- obj$options$mcR
      obj$extradata$parallel    <- obj$options$parallel
      
      obj$extradata[[obj$aim]]  <- NULL

      if (obj$filled)
                w <- which.min(obj$extradata$es)[1]
      else
                w<- 1
      obj$data                  <- obj$extradata[w,]
      obj$info$letter      <- "ME"
      obj$info$esmax       <- .9801
      obj$info$esmin       <-  1e-06
      obj$info$nmin        <-  10
      obj$info$nochecks    <-  "es"
      jinfo("Checking data for medcomplex done")
}


checkfailure <- function(obj, ...) UseMethod(".checkfailure")

.checkfailure.default <- function(obj,results) {


}




### additional check for models



commonchecks <- function(obj) {
  
    if (!obj$ok) return()
  
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

    if (is.something(obj$data$n ) ) {
        if ( any(obj$data$n < obj$info$nmin )) {
                   obj$stop(paste("N (total sample size) should be larger than",obj$info$nmin))
        }
    } 
  
    if (is.something(obj$info$eslbound) && is.something(obj$data$es))
         if (obj$info$eslbound > obj$data$es) obj$stop("The effect size " %+% obj$info$letter %+% " cannot be smaller than " %+% obj$info$eslbound)

    if (is.something(obj$info$esmax) && is.something(obj$data$es))
         if (obj$info$esmax < obj$data$es) obj$stop("The effect size " %+% obj$info$letter %+% " cannot be larger than " %+% obj$info$esmax)

    if (is.something(obj$data$es) && !obj$option("is_equi") ) {
        .data<-obj$data
        .data$n<-obj$info$nmax
        esmin<-find_min_es(obj,.data)
        fesmin<-format5(esmin)
        es<-format5(obj$data$es)


        if ( any(abs(obj$data$es) < abs(esmin))) {
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


   #  if (obj$data$es < obj$info$esmin) {
   #  
   #                 message<-paste0("The effect size (",obj$info$letter," = ", es,") is too small than the effect size (",obj$info$letter," = ", esmin,")",
   #                                 " That requires more than ",self$info$nmax_spell," cases (N=",self$info$nmax,")  to obtain a power of ",obj$data$power,".")
   #                 obj$warning<-list(topic="plotnotes",message=message,head="info")
   #  
   # }
    
  
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

