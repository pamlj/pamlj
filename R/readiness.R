

readiness <- function(options) {
  
  result=list(reason=FALSE,ready=TRUE,report=FALSE)

  # if (!is.something(trimws(options$dep))) 
  #        result <- list(reason = "<b>Required action</b>: <br> Please define a dependent variable",
  #                       ready = FALSE,
  #                       report = TRUE)
  
    
  return(result)
}
