### here are the S3 functions to fill the main tables


powertab_init <- function(obj, ...) UseMethod(".powertab_init")

.powertab_init.default <- function(obj) {
  
          if (!obj$ok) return()
  
          tab <-  obj$data
          if (!is.null(obj$data)) 
                 attr(tab,"titles")<-list(es=obj$info$letter)  
          return(tab)
          
}

.powertab_init.medcomplex <- function(obj) {

          if (!obj$ok) return()
  
          tab <-  obj$extradata
          attr(tab,"titles")<-list(es=obj$info$letter)  
          return(tab)
          
}

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
  
.powertab.medcomplex <- function(obj) {

   tab<-powervector(obj,obj$extradata)
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



powerbyn <- function(x, ...) UseMethod(".powerbyn")

.powerbyn.default <- function(obj) {

            power = c(.5, .8, .95)
            data<-obj$data
            dd<-do.call(rbind,lapply(power, function(x) {
                                 data$power<-x
                                 return(data)}))
            dd$n<-NULL    
            results<-powervector(obj,dd)
            results$n<-round(results$n,digits=0)

            esList <-list(
                          list(n=results$n[1]),
                          list(n=results$n[1] %+% "-" %+% results$n[2]),
                          list(n=results$n[2] %+% "-" %+% results$n[3]),
                          list(n=results$n[3])
                      )
            return(esList)
            
}



info_text <- function(obj, ...) {
  
text <- paste(
    '<style>',
    '.accordion {',
    '  background-color: #3498db;', 
    '  color: white;',
    '  cursor: pointer;',
    '  padding: 8px 15px;',
    '  width: 100%;',
    '  border: none;',
    '  text-align: left;',
    '  outline: none;',
    '  font-size: 16px;',
    '  transition: 0.4s;',
    '  display: flex;',
    '  align-items: center;',
    '  position: relative;',
    '  border-top-left-radius: 8px;',
    '  border-top-right-radius: 8px;',
    '}',
    '.accordion svg {',
    '  margin-right: 15px;',
    '  transition: fill 0.4s;',
    '}',
    '.accordion svg .circle {',
    '  fill: white;',
    '}',
    '.accordion svg .horizontal,',
    '.accordion svg .vertical {',
    '  fill: #3498db;',
    '  transition: transform 0.8s ease-in-out;',
    '  transform-origin: center;',
    '}',
    '.accordion.active svg .vertical {',
    '  transform: scaleY(0);',
    '}',
    '.panel {',
    '  padding: 0 15px;',
    '  display: none;',
    '  background-color: white;',
    '  overflow: hidden;',
    '}',
    '</style>',
    '<script>',
    'var acc = document.getElementsByClassName("accordion");',
    'for (var i = 0; i < acc.length; i++) {',
    '  acc[i].addEventListener("click", function() {',
    '    this.classList.toggle("active");',
    '    var panel = this.nextElementSibling;',
    '    if (panel.style.display === "block") {',
    '      panel.style.display = "none";',
    '    } else {',
    '      panel.style.display = "block";',
    '    }',
    '  });',
    '}',
    '</script>',
    '<button class="accordion">',
    '  <svg width="20" height="18" viewBox="0 0 24 24">',
    '    <circle class="circle" cx="12" cy="12" r="11" />',
    '    <rect class="horizontal" x="5" y="11" width="15" height="3" />',
    '    <rect class="vertical" x="11" y="5" width="3" height="15" />',
    '  </svg>',
    '  <span style="font-size: 14px;">Info</span>',
    '</button>',
    '<div class="panel">',
    '{addinfo}',
    '{addinfo2}',
    '{help}',
    '</div>'

)

  mode2<-ifelse(is.something(as.character(INFO2[[obj$mode]])),INFO2[[obj$mode]]," ")
  jmvcore::format(text, addinfo=INFO[obj$caller],addinfo2=mode2, help=link_help(obj))
  
  
  
}




extrainfo <- function(obj, ...) UseMethod(".extrainfo")

.extrainfo.default <-function(obj) return()

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


.extrainfo.propind <- function(obj) {
 
   if (!obj$option("explain")) return()  
  
   infoparms<-list(n="total sample size N=" %+% obj$data$n,
                   es="effect size " %+% obj$info$letter %+% " = " %+% format5(obj$data$es),
                   power="power equal to " %+% obj$data$power)
   
   infoparms[[obj$aim]]<-NULL

   text<-"<p> Power parameters are computed for a z-test with " %+%
          paste(infoparms, collapse=", ") %+% 
          " and type I error rate set to " %+% obj$data$sig.level %+%
          " The z-test compares two proportions in two different groups of cases." %+%
          " The proportions are transformed with the archsine function before computing the power parameters.</p>" %+%
            "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format5(obj$data[[obj$aim]]) %+% ".</p>"

    obj$warning<-list(topic="extrainfo",message=text,head="info")  

}

.extrainfo.propone <- function(obj) {
 
   if (!obj$option("explain")) return()  
  
   infoparms<-list(n="total sample size N=" %+% obj$data$n,
                   es="effect size " %+% obj$info$letter %+% " = " %+% format5(obj$data$es),
                   power="power equal to " %+% obj$data$power)
   
   infoparms[[obj$aim]]<-NULL

   text<-"<p> Power parameters are computed for a z-test with " %+%
          paste(infoparms, collapse=", ") %+% 
          " and type I error rate set to " %+% obj$data$sig.level %+%
          " The z-test tests one proportion (P1) obtained in the one sample against an hypothetical value (P2)." %+%
          " The proportions are transformed with the archsine function before computing the power parameters.</p>" %+%
            "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format5(obj$data[[obj$aim]]) %+% ".</p>"

    obj$warning<-list(topic="extrainfo",message=text,head="info")  

}

.extrainfo.proppaired <- function(obj) {
 
   if (!obj$option("explain")) return()  
  
   infoparms<-list(n="total sample size N=" %+% obj$data$n,
                   es="effect size " %+% obj$info$letter %+% " = " %+% format5(obj$data$es),
                   power="power equal to " %+% obj$data$power)
   
   infoparms[[obj$aim]]<-NULL

   text<-"<p> Power parameters are computed for a (approximated) McNemar test with " %+%
          paste(infoparms, collapse=", ") %+% 
          " and type I error rate set to " %+% obj$data$sig.level %+%
          " The McNemar tests two proportions obtained in the same sample." %+%
            "<p>The required " %+% nicify_param(obj$aim) %+% " is " %+% nicify_param(obj$aim,short=TRUE) %+% "=" %+% format5(obj$data[[obj$aim]]) %+% ".</p>"

    obj$warning<-list(topic="extrainfo",message=text,head="info")  

}


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

