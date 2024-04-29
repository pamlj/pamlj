
effectsize <- function(obj, ...) UseMethod(".effectsize")

.effectsize.default <- function(obj) return()

.effectsize.beta <- function(obj) {

  if (nrow(obj$analysis$data)==0)
    return(list(
               list(index=letter_r2),
               list(index="Effect tollerance"),
               list(index=paste("Effect",letter_peta2)),
               list(index="Model df")
                 ))
  
   tab <- list()
   ladd(tab)<-list(value=obj$data$r2)
   tol<-1-obj$data$ri2
   ladd(tab)<-list(value=tol)
   peta<-obj$data$f2/(1+obj$data$f2)
   ladd(tab)<-list(value=peta)
   ladd(tab)<-list(value=obj$data$df_model)
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
