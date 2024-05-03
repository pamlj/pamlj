
effectsize_init <- function(obj, ...) UseMethod(".effectsize_init")
effectsize_run <- function(obj, ...) UseMethod(".effectsize_run")

.effectsize_init.default <- function(obj) return()
.effectsize_run.default <- function(obj) return()


.effectsize_init.beta <- function(obj) {

    return(list(
               list(index=letter_r2),
               list(index="Effect tollerance"),
               list(index=paste("Effect",letter_peta2)),
               list(index="Model df")
                 ))
}

.effectsize_run.beta <- function(obj) {

   tab <- list()
   ladd(tab)<-list(value=obj$data$r2)
   tol<-1-obj$data$ri2
   ladd(tab)<-list(value=tol)
   peta<-obj$data$f2/(1+obj$data$f2)
   ladd(tab)<-list(value=peta)
   ladd(tab)<-list(value=obj$data$df_model)
   return(tab)
  
}


.effectsize_init.eta <- function(obj) {

   tab <- list()
   ladd(tab)<-list(index=letter_r2,value=obj$data$r2)
   peta<-obj$data$f2/(1+obj$data$f2)
   if (length(peta)==0) peta<-"."
   ladd(tab)<-list(index=paste("Effect",letter_peta2),value=peta)
   return(tab)
  
}

.effectsize_run.eta <- function(obj) {

   .effectsize_init.eta(obj)
  
}
