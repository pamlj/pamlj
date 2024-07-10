
find_min_n <- function(obj, ...) UseMethod(".find_min_n")

.find_min_n.default <- function(obj,data) {
  
  
  jinfo("Finding max n")

  data$es<- obj$info$esmax
  data$n <- NULL
  res<-try_hard(powervector(obj,data))
  if (isFALSE(res$error)) 
    n<-ceiling(res$obj$n)
  else
    n<-obj$info$nmin

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
    stop("there must be a max n")
return(n)
}


find_max_es <- function(obj, ...) UseMethod(".find_max_es")

.find_max_es.default <- function(obj,data) {

  data$power=.99
  data$es <- NULL
  res<-try_hard(powervector(obj,data))
  if (isFALSE(res$error)) 
    es<-res$obj$es
  else
    stop("there must be a max es")
return(es)
}


required_param<-function(data) {

  whichnull<-setdiff(c("n","es","sig.level","power"), names(data))  
  if (length(whichnull)>1 || length(whichnull)==0)
         stop("PAMLj: only one parameters should be NULL")
  whichnull
}
