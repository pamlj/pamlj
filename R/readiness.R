

readiness <- function(options) {
  
    result=list(reason=FALSE,ready=TRUE,report=FALSE)

    test<-unlist(rlist::list.map(options$fixed_sizes, as.numeric(value)>0))  
    test[is.na(test)]<-FALSE

    if (!any(test))
        result <- list(reason = "<b>Required action</b>: <br> Please enter the effect size for the effects",
                   ready = FALSE,
                   report = TRUE)

  if (!is.something(options$model_terms)) 
    result <- list(reason = "<b>Required action</b>: <br> Please define at least one independent variable",
                   ready = FALSE,
                   report = TRUE)
  
  if (!is.something(trimws(options$dep))) 
         result <- list(reason = "<b>Required action</b>: <br> Please define a dependent variable",
                        ready = FALSE,
                        report = TRUE)
  
  
  mark(length(options$model_terms))
  
  
  return(result)
}
