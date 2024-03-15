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


.powerfunction.glm <- function(obj) {
  
    if (is.something(obj$input$n)) 
      v<-obj$input$n-obj$input$df_model-1
    else
      v<-NULL
    res<-pwr::pwr.f2.test(f2=obj$input$aes,u=obj$input$df_effect,v=v,power=obj$input$power,sig.level=obj$input$alpha)
    obj$data[["n"]]<-round(res$v+obj$input$df_model+1)
    obj$data[["es"]]<-obj$fromaes(res$f2)
    obj$data[["aes"]]<-res$f2
    obj$data[["alpha"]]<-res$sig.level
    obj$data[["power"]]<-res$power
    obj$data[["df1"]]<-res$u
    obj$data[["df2"]]<-res$v
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
                          list(es=paste(probs_es[1],'<', greek_vector["rho"], greek_vector["leq"],probs_es[2])),
                          list(es=paste(probs_es[1],'<', greek_vector["rho"], greek_vector["leq"],probs_es[3])),
                          list(es=paste(greek_vector["rho"], greek_vector["geq"],probs_es[3]))
            )

            return(esList)
            
}

.powerbyes.glm <- function(obj) {

            probs = c(.5, .8, .95)
            probs_es = sapply(probs, function(p){
              v<-obj$data$n-obj$data$df_model-1
              pwr::pwr.f2.test(u=obj$data$df_effect,v=v,
                                 sig.level = obj$data$alpha, power = p)$f2
           })
            probs_es<-obj$fromaes(probs_es)
            probs_es<-round(probs_es,digits=3)
            esList <-list(list(es=paste('0 <', obj$data$letter, greek_vector["leq"],probs_es[1])),
                          list(es=paste(probs_es[1],'<', obj$data$letter, greek_vector["leq"],probs_es[2])),
                          list(es=paste(probs_es[2],'<', obj$data$letter, greek_vector["leq"],probs_es[3])),
                          list(es=paste(obj$data$letter,">" ,probs_es[3]))
            )

            return(esList)
            
}



## powervector must accept a runner object and a list of data. It must be able to return either a vector
## of parameters as a function of a vector in data, or a single parameter if no vector is found in input
## for this function the effect size is assumed to be the one original one, so not transformed (when necessary)
## and returns the actual effect size (transformed back)

powervector <- function(obj, ...) UseMethod(".powervector")


.powervector.correlation <- function(obj,data) {
                
                .data <- data
                names(.data) <- fields_tothem(obj,names(.data))
                whichnull<-setdiff(c("n","r","sig.level","power"), names(.data))  
                if (length(whichnull)>1)
                   stop("FUNCTION powervector: only one parameters should be NULL")
                if (length(whichnull)==0)
                   stop("FUNCTION powervector: exactly one parameters should be NULL")
                tails<-obj$tails
                ## for some reason, if n is a vector and the effect size is asked, it gives an error
                if (whichnull=="r" && length(data$n)>1)
                   results<-sapply(data$n, function(x) pwr::pwr.r.test(n=x,power=data$power,sig.level=data$alpha,alternative=tails)[["r"]] )
                else
                   results <- pwr::pwr.r.test(n=data$n,r=data$es,power=data$power,sig.level=data$alpha,alternative=tails)[[whichnull]]
                return(results)

  
  
}



.powervector.glm <- function(obj,data) {
                
               
                data[["sig.level"]]<-data$alpha
                
                if (is.something(data$n))
                   data[["v"]]<- data$n - obj$data$df_model -1
                
                u <- obj$data$df_effect
                if (is.something(data$es))
                                     data[["f2"]]<-obj$toaes(data$es)

                whichnull<-setdiff(c("v","f2","sig.level","power"), names(data))  
                if (length(whichnull)>1)
                   stop("FUNCTION powervecot: only one parameters should be NULL")
                if (length(whichnull)==0)
                   stop("FUNCTION powervector: exactly one parameters should be NULL")
                ## for some reason, if v is a vector and the effect size is asked, it gives an error
                if (whichnull=="f2" && length(data$v)>1) {
                   results<-sapply(data$v, function(x) pwr::pwr.f2.test(u=u,v=x,power=data$power,sig.level=data$alpha)[[whichnull]])
                }
                else
                   results <- pwr::pwr.f2.test(u=u,v=data$v,f2=data$f2,power=data$power,sig.level=data$alpha)[[whichnull]]

                if (whichnull=="v")  results<-round(results+obj$data$df_model+1)
                if (whichnull=="f2") results<-obj$fromaes(results)
                return(results)

}



