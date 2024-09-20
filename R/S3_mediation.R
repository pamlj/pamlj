## checkdata:       (required) this prepares all the info required to estimate the power parameters

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

        if (obj$aim == "es") obj$stop("Finding minumum effect size for complex mediation models is not implemented yet")
  
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
                          corMat[2,3] <- corMat[3,2] <- plotdata$r12
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
                          corMat[3,5] <- corMat[5,3] <- plotdata$a2*plotdata$cprime + plotdata$b2 + plotdata$b1*plotdata$r12 + plotdata$b3*plotdata$r23
                          corMat[4,5] <- corMat[5,4] <- plotdata$a3*plotdata$cprime + plotdata$b3 + plotdata$b2*plotdata$r23 + plotdata$b1*plotdata$r13
  
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
                         corMat[3,4] <- corMat[4,3] <- plotdata$a2*plotdata$cprime + plotdata$b2 + plotdata$b1*plotdata$d1 + plotdata$a1*plotdata$cprime*plotdata$d1
                         
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
                         obj$warning<-list(topic="powertab",message="Additional coefficients: d1=" %+% plotdata$d1)
 

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

## powervector:     (required) pass the data, with adjutment, to the lowerlevel power functions 

.powervector.mediation <- function(obj,data) {

                 aim<-required_param(data)
                 if (aim=="es") data$a<-NULL

                 ## dealing with seed for montecarlo
                 seed<-NULL
                 if (obj$options$set_seed) data$seed=obj$options$seed
                 results<-lapply(1:nrow(data),function(i) {
                     
                     test      <- data$test[i]
                     if (test=="mc") fun<-pamlj.mediation.mc
                     else fun<-pamlj.mediation
                     
#                    .names <- intersect(names(data),rlang::fn_fmls_names(fun))
#                     one      <- data[i,.names]
                      one      <- data[i,]
                      one      <- one[!sapply(one,is.na)]
                     tryobj<-try_hard(do.call(fun,one), silent=F)
                     out<-tryobj$obj
                     if (!isFALSE(tryobj$error)) {
                     switch(aim,
                            n = {
                                stop("failed on n")
                               },
                            power={ 
                                stop("failed on power")
                                  },
                            es={ 
                               stop("failed on es")
                               }
                            
                            )
                     }
                     out
                    })
                          

                 results<-as.data.frame(do.call("rbind",results))
               
                 if (nrow(results)>3) results<- na.omit(results)
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 
                .names<-c(names(data)[!names( data) %in% names(results)],names(results))
                 odata<- data[, !names( data) %in% names(results)]
                 results<-cbind(odata,results)
                 names(results)<-.names
                 results$n  <- round(results$n,digits=0)
                 return(results)
  
}



## powertab_init:   (not required) this function produces or format the main table, powertab, before running

.powertab_init.medcomplex <- function(obj) {

          if (!obj$ok) return()
  
          tab <-  obj$extradata
          attr(tab,"titles")<-list(es=obj$info$letter)  
          return(tab)
          
}



## powertab:        (not required) this function produces or format the main table, powertab, after running

.powertab.medcomplex <- function(obj) {

   tab<-powervector(obj,obj$extradata)
   return(tab)
}


## powerbyes:       this function produces or format the powerbyes table , after the estimation is done
## powerbyen:       this function produces or format the powerbyen table , after the estimation is done
## effectsize_init: (not required) some sub.module requires additional effect size to be computed. This function inits the table

.effectsize_init.medsimple <- function(obj) {

    return(list(
               list(index="ME"),
               list(index=letter_r2 %+% " predicting M"),
               list(index=letter_r2 %+% " predicting Y"),
               list(index=" X-Y correlation (c) ")
                 ))
}


.effectsize_init.medcomplex <- function(obj) {

    if (obj$options$model_type != "threemeds" ) {
    return(list(
               list(index=letter_r2 %+% " predicting M1"),
               list(index=letter_r2 %+% " predicting M2"),
               list(index=letter_r2 %+% " predicting Y"),
               list(index=" X-Y correlation (c) ")
               
                 ))
    } else
       return(list(
               list(index=letter_r2 %+% " predicting M1"),
               list(index=letter_r2 %+% " predicting M2"),
               list(index=letter_r2 %+% " predicting M3"),
               list(index=letter_r2 %+% " predicting Y"),
               list(index=" X-Y correlation (c) ")
               
                 ))
    
}


## effectsize_run:  (not required) some sub.module requires additional effect size to be computed. This function fills the table

.effectsize_run.medsimple <- function(obj) {

   tab <- list()
   ladd(tab) <- list(value=obj$data$es)
   ladd(tab) <- list(value=obj$data$r2a)
   ladd(tab) <- list(value=obj$data$r2y)
   ladd(tab) <- list(value=obj$info$rxy)
   
   return(tab)
  
}

.effectsize_run.medcomplex <- function(obj) {

  if (obj$options$model_type == "twoserial" ) {
    return(list(
               list(value=obj$extradata$r2a[1]),
               list(value=obj$extradata$r2d1[3]),
               list(value=obj$extradata$r2y[2]),
               list(value=obj$info$rxy)
 

                 ))
  }
    if (obj$options$model_type == "twomeds" ) {
    return(list(
               list(value=obj$extradata$r2a[1]),
               list(value=obj$extradata$r2a[2]),
               list(value=obj$extradata$r2y[1]),
               list(value=obj$info$rxy)

                 ))
  }

    if (obj$options$model_type == "threemeds" ) {
    return(list(
               list(value=obj$extradata$r2a[1]),
               list(value=obj$extradata$r2a[2]),
               list(value=obj$extradata$r2a[3]),
               list(value=obj$extradata$r2y[1]),
               list(value=obj$info$rxy)

                 ))
  }

   return(tab)
  
}



## extrainfo:       (better if is there) this function add some extra info to be given to the user when the option "explain" is selected

.extrainfo.mediation <- function(obj) {
 
   if (!obj$option("explain")) return()  
  
   switch (obj$data$test,
           sobel = test <-"for the <b>Sobel test (z-test)</b>",
           joint = test <- "<b> for joint significance test </b> (both a and b significant)",
           mc    = test <- "with <b>Monte Carlo simulation method</b>"
   )
    infoparms<-list(n="total sample size N=" %+% obj$data$n,
                   power="power equal to " %+% format5(obj$data$power)
                   )

   if (is.null(obj$extradata)) 
            infoparms$es <- "completely standardized effect size " %+% obj$info$letter %+% " = " %+% format5(obj$data$es) %+% " given by a*b=" %+% format5(obj$data$a) %+% "*" %+% format5(obj$data$b)
      else 
            infoparms$es<- "mediated effects" %+% paste(obj$extradata$effect,collapse=", ")
  
   
   infoparms[[obj$aim]]<-NULL

   text<-"<p> Power parameters are computed " %+% test %+% " with " %+%
          paste(infoparms, collapse=", ") %+% 
          " and type I error rate set to " %+% obj$data$sig.level %+% "." %+%
          " The test tests whether the mediated effect is different from zero." %+%
            "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format5(obj$data[[obj$aim]]) %+% "."
   
    if (obj$aim == "es") text <- text %+% " The required X to mediation coefficient (a) is " %+% format5(obj$data$a) %+% " yielding a power equal to " %+%  format5(obj$data$power) %+% "."

     text <- text %+% "</p>"
     
     if (is.something(obj$info$ryxpower)) text <- text %+%
                                                 "<p> Given the results, the test concerning the simple regression between X and Y will have power equal to " %+%
                                                  obj$info$ryxpower %+% 
                                                 "<p>"
     
    obj$warning<-list(topic="extrainfo",message=text,head="info")  

}

### find parameters need some specs for mediation

.find_min_n.mediation <- function(obj,data) {
  
  data$n <- NULL
  data$a<-ifelse(data$a^(.5) < .99, data$a^(.5), .99)
  res<-try_hard(powervector(obj,data))
  if (isFALSE(res$error)) {
    n<-ceiling(res$obj$n)
  }
  else
    n<-obj$info$nmin
   return(n)
}

