
effectsize_init <- function(obj, ...) UseMethod(".effectsize_init")

effectsize_run <- function(obj, ...) UseMethod(".effectsize_run")

.effectsize_init.default <- function(obj) {
    mark("No effect size to add for class",class(obj))
    return()
  }
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
   ladd(tab)<-list(value=obj$info$r2)
   tol<-1-obj$info$ri2
   ladd(tab)<-list(value=tol)
   peta<-obj$data$f2/(1+obj$data$f2)
   ladd(tab)<-list(value=peta)
   ladd(tab)<-list(value=obj$data$df_model)
   return(tab)
  
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

.effectsize_run.eta <- function(obj) {

   .effectsize_init.eta(obj)
  
}


.effectsize_init.factorial <- function(obj) {

   tab <- list()
   ladd(tab)<-list(index=paste("Model",letter_r2),value=obj$info$r2)
   if (obj$option("esos"))
             ladd(tab)<-list(index=paste0("Error term (",greek_vector["sigma"],")"),value=obj$info$sigma)
   ladd(tab)<-list(index="Model df",value=obj$data$df_model)

   return(tab)
  
}

.effectsize_run.factorial <- function(obj) {

   .effectsize_init.factorial(obj)
  
}


.effectsize_init.medsimple <- function(obj) {

    return(list(
               list(index="ME"),
               list(index=letter_r2 %+% " predicting M"),
               list(index=letter_r2 %+% " predicting Y")
                 ))
}

.effectsize_run.medsimple <- function(obj) {

   tab <- list()
   ladd(tab)<-list(value=obj$data$es)
   ladd(tab)<-list(value=obj$data$r2a)
   ladd(tab)<-list(value=obj$data$r2y)
   
   return(tab)
  
}

.effectsize_init.medcomplex <- function(obj) {

    if (obj$options$model_type != "threemeds" ) {
    return(list(
               list(index=letter_r2 %+% " predicting M1"),
               list(index=letter_r2 %+% " predicting M2"),
               list(index=letter_r2 %+% " predicting Y")
                 ))
    } else
       return(list(
               list(index=letter_r2 %+% " predicting M1"),
               list(index=letter_r2 %+% " predicting M2"),
               list(index=letter_r2 %+% " predicting M3"),
               list(index=letter_r2 %+% " predicting Y")
                 ))
    

}

.effectsize_run.medcomplex <- function(obj) {

  if (obj$options$model_type == "twoserial" ) {
    return(list(
               list(value=obj$extradata$r2a[1]),
               list(value=obj$extradata$r2d1[3]),
               list(value=obj$extradata$r2y[2])

                 ))
  }
    if (obj$options$model_type == "twomeds" ) {
    return(list(
               list(value=obj$extradata$r2a[1]),
               list(value=obj$extradata$r2a[2]),
               list(value=obj$extradata$r2y[1])

                 ))
  }

    if (obj$options$model_type == "threemeds" ) {
    return(list(
               list(value=obj$extradata$r2a[1]),
               list(value=obj$extradata$r2a[2]),
               list(value=obj$extradata$r2a[3]),
               list(value=obj$extradata$r2y[1])

                 ))
  }

   return(tab)
  
}

