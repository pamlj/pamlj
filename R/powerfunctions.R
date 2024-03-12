### computes the actual power parameters

powerfunction <- function(x, ...) UseMethod(".powerfunction")


.powerfunction.correlation <- function(obj) {
  

    if (obj$tails == "two") 
           alt="two.sided"
    else {
      if (obj$data[["es"]]>0)
          alt="greater"
      else 
          alt="less"
    }
    res<-pwr::pwr.r.test(n=obj$input[["n"]],r=obj$input[["es"]],power=obj$input[["power"]],sig.level=obj$input[["alpha"]],alternative=alt)
    obj$data[["n"]]<-round(res$n)
    obj$data[["es"]]<-res$r
    obj$data[["alpha"]]<-res$sig.level
    obj$data[["power"]]<-res$power

    warning(res$method)
    return(obj$data)
}





### computes the  power effect sizes

powerbyes <- function(x, ...) UseMethod(".powerbyes")


.powerbyes.correlation <- function(obj) {

            probs = c(.5, .8, .95)
            probs_es = sapply(probs, function(p){
              pwr::pwr.r.test(obj$data$n,
                                 sig.level = obj$data$alpha, power = p,
                                 alternative = obj$tails)$r
           })
            probs_es<-round(probs_es,digits=3)
            esList <-list(list(es=paste('0 <', greek_vector["rho"], greek_vector["leq"],probs_es[1])),
                          list(es=paste(probs_es[2],'<', greek_vector["rho"], greek_vector["leq"],probs_es[2])),
                          list(es=paste(probs_es[3],'<', greek_vector["rho"], greek_vector["leq"],probs_es[3])),
                          list(es=paste(greek_vector["rho"], greek_vector["geq"],probs_es[3]))
            )

            return(esList)
            
}

## powervector must accept a runner object and a list of data. It must be able to return either a vector
## of parameters as a function of a vector in data, or a single parameter if no vector is found in input

powervector <- function(obj, ...) UseMethod(".powervector")


.powervector.correlation <- function(obj,data) {
                
                .data <- data
                names(.data) <- fields_tothem(obj,names(.data))
                whichnull<-setdiff(c("n","r","sig.level","power"), names(.data))  
                if (length(whichnull)>1)
                   stop("FUNCTION freepower: only one parameters should be NULL")
                if (length(whichnull)==0)
                   stop("FUNCTION freepower: exactly one parameters should be NULL")
                tails<-obj$tails
                ## for some reason, if n is a vector and the effect size is asked, it gives an error
                if (whichnull=="r" && length(data$n)>1)
                   results<-sapply(data$n, function(x) pwr::pwr.r.test(n=x,power=data$power,sig.level=data$alpha,alternative=tails)[["r"]] )
                else
                   results <- pwr::pwr.r.test(n=data$n,r=data$es,power=data$power,sig.level=data$alpha,alternative=tails)[[whichnull]]
              
              
                return(results)

  
  
}





