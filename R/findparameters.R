
find_min_n <- function(obj, ...) UseMethod(".find_min_n")

.find_min_n.default <- function(obj,data) {
  
  
  jinfo("Finding min n")

  data$es<- obj$info$esmax
  data$n <- NULL
  res<-try_hard(powervector(obj,data))
  if (isFALSE(res$error)) 
    n<-ceiling(res$obj$n)
  else
    n<-obj$info$nmin
   jinfo("...done")
   return(n)
}


find_max_n <- function(obj, ...) UseMethod(".find_max_n")

.find_max_n.default <- function(obj,data) {
  
  jinfo("Finding max n")
  data$es<-ifelse(data$es*.95 > obj$info$esmin, data$es*.95, obj$info$esmin)
  data$power=.99
  data$n <- NULL
  res<-try_hard(powervector(obj,data))
  if (isFALSE(res$error)) 
    n<-ceiling(res$obj$n)
  else
    n<-obj$info$nmax
  jinfo("...done")
  
return(n)
}


find_max_es <- function(obj, ...) UseMethod(".find_max_es")

.find_max_es.default <- function(obj,data) {

  jinfo("Finding max es")
  data$es <- NULL
#  data$n  <- obj$info$nmin
  res<-try_hard(powervector(obj,data))
  if (isFALSE(res$error)) 
    es<-res$obj$es
  else
    es<-obj$info$esmax
return(es)
}

find_min_es <- function(obj, ...) UseMethod(".find_min_es")

.find_min_es.default <- function(obj,data) {

  jinfo("Finding min es")

  data$es <- NULL
  data$n  <- obj$info$nmax
  res<-try_hard(powervector(obj,data))
  if (isFALSE(res$error)) 
    es<-res$obj$es
  else
    stop("there must be a min es")
return(es)
}

required_param<-function(data) {

  whichnull<-setdiff(c("n","es","sig.level","power"), names(data))  
  if (length(whichnull)>1 || length(whichnull)==0)
         stop("PAMLj: only one parameters should be NULL")
  whichnull
}
