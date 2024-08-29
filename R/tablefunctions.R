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



