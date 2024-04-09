
find_min_n <- function(obj, ...) UseMethod(".find_min_n")

.find_min_n.default <- function(obj,data) {
  
  data$es<-obj$data$esmax
  data$n <- NULL
  res<-powervector(obj,data)
  ceiling(res$n)
}