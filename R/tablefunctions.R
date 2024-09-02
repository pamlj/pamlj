### here are the S3 functions to fill the main tables


powertab <- function(obj, ...) UseMethod(".powertab")

.powertab.default <- function(obj) return(obj$data)


.powertab.facpeta <- function(obj) {

   if (any(obj$data$n!=obj$data$nb))
                    warning("N per group (N-group) is adjusted to obtain a balanced design.")

   return(obj$data)
   
}

.powertab.factorial <- function(obj) {

   tab<-powervector(obj,obj$extradata)
   attr(tab,"titles")<-list(es=letter_peta2)
   if (any(tab$n!=tab$nb))
                    warning("N per group (N-group) is adjusted to obtain a balanced design.")
   warning("Model df=",tab$df_model[1])
   return(tab)
}
  

powerbyes <- function(x, ...) UseMethod(".powerbyes")

.powerbyes.default <- function(obj) {

            power = c(.5, .8, .95)
            data<-obj$data
            data$power<-NULL
            suppressWarnings(dd<-as.data.frame(cbind(power,data)))
            dd$es<-NULL
            res<-powervector(obj,dd)
            probs_es<-format(res$es,digits=3)
            check<-which(is.na(res$es))
            if (length(check)>0) warning("Some effect size cannot be computed given the input parameters.")

            esList <-list(list(es=paste('0 <', obj$info$letter, greek_vector["leq"],probs_es[1])),
                          list(es=paste(probs_es[1],'<', obj$info$letter, greek_vector["leq"],probs_es[2])),
                          list(es=paste(probs_es[2],'<', obj$info$letter, greek_vector["leq"],probs_es[3])),
                          list(es=paste(obj$info$letter,">" ,probs_es[3]))
            )
            return(esList)
            
}


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


extrainfo <- function(obj, ...) UseMethod(".extrainfo")

.extrainfo.default <-function(obj) return()

.extrainfo.ttestind <- function(obj) {
 
   if (!obj$option("explain")) return()  
  
   infoparms<-list(n="total sample size N=" %+% obj$data$n,
                   es="effect size " %+% obj$info$letter %+% " = " %+% format(obj$data$es,digits=5),
                   power="power equal to " %+% obj$data$power)
   
   infoparms[[obj$aim]]<-NULL

   text<-"<p> Power parameters are computed for a t-test with " %+%
          paste(infoparms, collapse=", ") %+% 
          " and type I error rate set to " %+% obj$data$sig.level %+%
          " The t-test compares the means of two groups of cases.</p>" %+%
            "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format(obj$data[[obj$aim]],digits=5) %+% ".</p>"

    obj$warning<-list(topic="extrainfo",message=text,head="info")  

}

.extrainfo.ttestpaired <- function(obj) {
  
   if (!obj$option("explain")) return()  
       
   infoparms<-list(n="total sample size N=" %+% obj$data$n,
                   es="effect size " %+% obj$info$letter %+% " = " %+% format(obj$data$es,digits=5),
                   power="power equal to " %+% obj$data$power)
   
   infoparms[[obj$aim]]<-NULL

   text<-"<p> Power parameters are computed for a t-test with " %+%
          paste(infoparms, collapse=", ") %+% 
          " and type I error rate set to " %+% obj$data$sig.level %+%
          " The t-test compares two means in a repeated-measure design.</p>" %+%
            "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format(obj$data[[obj$aim]],digits=5) %+% ".</p>"
      obj$warning<-list(topic="extrainfo",message=text,head="info")  
}

.extrainfo.ttestone <- function(obj) {
  
   if (!obj$option("explain")) return()  

   infoparms<-list(n="total sample size N=" %+% obj$data$n,
                   es="effect size " %+% obj$info$letter %+% " = " %+% format(obj$data$es,digits=5),
                   power="power equal to " %+% obj$data$power)
   
   infoparms[[obj$aim]]<-NULL

   text<-"<p> Power parameters are computed for a t-test with " %+%
          paste(infoparms, collapse=", ") %+% 
          " and type I error rate set to " %+% obj$data$sig.level %+%
          " The t-test tests the mean of one sample against the value of zero.</p>" %+%
            "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format(obj$data[[obj$aim]],digits=5) %+% ".</p>"
      obj$warning<-list(topic="extrainfo",message=text,head="info")  
}


.extrainfo.glm <- function(obj) {

  if (!obj$option("explain")) return()  
  
  if (obj$options$covs==0 && obj$options$factors==0) {
    
    terms<-ifelse(obj$data$df_model>1,"terms","term")
    text<-" <p> Power parameters are computed for a general linear model with " %+% obj$data$df_model %+%
          " degrees of freedom, for an effect size " %+% obj$info$letter %+% " = " %+% format(obj$data$es,digits=5) %+%
          " with " %+% obj$data$df_effect %+% " degrees of freedom " %+%
          " and type I error rate set to " %+% obj$data$sig.level %+%
          " The model is equivalent to a regression with " %+% obj$data$df_model %+% " " %+% terms %+% " or" %+%
          " an ANOVA with " %+% (obj$data$df_model+1) %+% " groups.</p>" %+%
          "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format(obj$data[[obj$aim]],digits=5) %+% ".</p>"
            
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
           mark(levels)
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
          " degrees of freedom, for an effect size " %+% obj$info$letter %+% " = " %+% format(obj$data$es,digits=5) %+%
          " with " %+% obj$data$df_effect %+% " degrees of freedom " %+%
          " and type I error rate set to " %+% obj$data$sig.level %+%
          " The model is equivalent to " %+% analysis %+% " with  terms: " %+%
            ifelse(is.null(feffects),"",feffects) %+%
            ifelse(is.null(reffects),"",reffects) %+%
            ifelse(is.null(meffects),"",meffects) %+%
          "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format(obj$data[[obj$aim]],digits=5) %+% ".</p>"
      obj$warning<-list(topic="extrainfo",message=text,head="info")
      

  }
  

  
}


.extrainfo.propind <- function(obj) {
 
   if (!obj$option("explain")) return()  
  
   infoparms<-list(n="total sample size N=" %+% obj$data$n,
                   es="effect size " %+% obj$info$letter %+% " = " %+% format(obj$data$es,digits=5),
                   power="power equal to " %+% obj$data$power)
   
   infoparms[[obj$aim]]<-NULL

   text<-"<p> Power parameters are computed for a z-test with " %+%
          paste(infoparms, collapse=", ") %+% 
          " and type I error rate set to " %+% obj$data$sig.level %+%
          " The z-test compares two proportions in two different groups of cases." %+%
          " The proportions are transformed with the archsine function before computing the power parameters.</p>" %+%
            "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format(obj$data[[obj$aim]],digits=5) %+% ".</p>"

    obj$warning<-list(topic="extrainfo",message=text,head="info")  

}

.extrainfo.propone <- function(obj) {
 
   if (!obj$option("explain")) return()  
  
   infoparms<-list(n="total sample size N=" %+% obj$data$n,
                   es="effect size " %+% obj$info$letter %+% " = " %+% format(obj$data$es,digits=5),
                   power="power equal to " %+% obj$data$power)
   
   infoparms[[obj$aim]]<-NULL

   text<-"<p> Power parameters are computed for a z-test with " %+%
          paste(infoparms, collapse=", ") %+% 
          " and type I error rate set to " %+% obj$data$sig.level %+%
          " The z-test tests one proportion (P1) obtained in the one sample against an hypothetical value (P2)." %+%
          " The proportions are transformed with the archsine function before computing the power parameters.</p>" %+%
            "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format(obj$data[[obj$aim]],digits=5) %+% ".</p>"

    obj$warning<-list(topic="extrainfo",message=text,head="info")  

}

.extrainfo.proppaired <- function(obj) {
 
   if (!obj$option("explain")) return()  
  
   infoparms<-list(n="total sample size N=" %+% obj$data$n,
                   es="effect size " %+% obj$info$letter %+% " = " %+% format(obj$data$es,digits=5),
                   power="power equal to " %+% obj$data$power)
   
   infoparms[[obj$aim]]<-NULL

   text<-"<p> Power parameters are computed for a (approximated) McNemar test with " %+%
          paste(infoparms, collapse=", ") %+% 
          " and type I error rate set to " %+% obj$data$sig.level %+%
          " The McNemar tests two proportions obtained in the same sample." %+%
            "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format(obj$data[[obj$aim]],digits=5) %+% ".</p>"

    obj$warning<-list(topic="extrainfo",message=text,head="info")  

}
