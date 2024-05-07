
find_min_n <- function(obj, ...) UseMethod(".find_min_n")

.find_min_n.default <- function(obj,data) {
  
  data$es<- data$esmax
  data$n <- NULL
  res<-try_hard(powervector(obj,data))
  if (isFALSE(res$error)) 
    n<-ceiling(res$obj$n)
  else
    n<-obj$nmin
return(n)
}

