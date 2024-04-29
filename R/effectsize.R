
effectsize <- function(obj, ...) UseMethod(".effectsize")

.effectsize.default <- function(obj) return()

.effectsize.beta <- function(obj) {

  
   tab <- list()
   ladd(tab)<-list(index=letter_r2,value=obj$data$r2)
   ri2<-1-obj$extradata$ri2
   if (is.null(ri2)) ri2<-"."
   ladd(tab)<-list(index="Effect tollerance",value=ri2)
   peta<-obj$data$f2/(1+obj$data$f2)
   if (length(peta)==0) peta<-"."
   ladd(tab)<-list(index=paste("Effect",letter_peta2),value=peta)
   return(tab)
  
}

.effectsize.eta <- function(obj) {

  
   tab <- list()
   ladd(tab)<-list(index=letter_r2,value=obj$data$r2)
   peta<-obj$data$f2/(1+obj$data$f2)
   if (length(peta)==0) peta<-"."
   ladd(tab)<-list(index=paste("Effect",letter_peta2),value=peta)
   return(tab)
  
}
