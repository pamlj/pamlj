### computes the actual power parameters

powerfunction <- function(x, ...) UseMethod(".powerfunction")


.powerfunction.ttestind <- function(obj) {
  
    if (obj$tails == "two") 
           alt="two.sided"
    else {
          alt="greater"
    }
    
    res<-pwr.t2n.test(n=obj$input[["n"]],r=obj$input[["es"]],power=obj$input[["power"]],sig.level=obj$input[["alpha"]],alternative=alt)
    obj$data[["n"]]<-round(res$n)
    obj$data[["es"]]<-res$r
    obj$data[["alpha"]]<-res$sig.level
    obj$data[["power"]]<-res$power

    warning(res$method)
    return(obj$data)
}

.powerfunction.correlation <- function(obj) {
  
    if (obj$tails == "two") 
           alt="two.sided"
    else {
          alt="greater"
    }

    res <-  pwr::pwr.r.test(n=obj$input[["n"]],r=obj$input[["es"]],power=obj$input[["power"]],sig.level=obj$input[["alpha"]],alternative=alt)

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
    
    u <- obj$input$df_effect
    
    f2<-obj$input$aes

    res<-pamlj.glm(f2=f2,u=u,v=v,power=obj$input$power,alpha=obj$data$alpha,df_model=obj$data$df_model, gpower=obj$options$gncp,tails=obj$tails)
    obj$data[["aes"]]<-res$f2
    obj$data[["n"]]<-round(res$n)
    obj$data[["es"]]<-obj$fromaes(res$f2)
    obj$data[["alpha"]]<-res$alpha
    obj$data[["power"]]<-res$power
    obj$data[["df1"]]<-res$u
    obj$data[["df2"]]<-round(res$v)
    return(obj$data)
}


### computes the  power effect sizes

powerbyes <- function(x, ...) UseMethod(".powerbyes")


.powerbyes.correlation <- function(obj) {

      if (obj$tails == "two") 
           alt="two.sided"
       else {
          alt="greater"
       }

            probs = c(.5, .8, .95)
            probs_es = sapply(probs, function(p){
              pwr::pwr.r.test(obj$data$n,
                                 sig.level = obj$data$alpha, power = p,
                                 alternative = alt)$r
           })
            probs_es<-round(probs_es,digits=3)
            esList <-list(list(es=paste('0 <', greek_vector["rho"], greek_vector["leq"],probs_es[1])),
                          list(es=paste(probs_es[1],'<', greek_vector["rho"], greek_vector["leq"],probs_es[2])),
                          list(es=paste(probs_es[2],'<', greek_vector["rho"], greek_vector["leq"],probs_es[3])),
                          list(es=paste(greek_vector["rho"], greek_vector["geq"],probs_es[3]))
            )

            return(esList)
            
}

.powerbyes.glm <- function(obj) {

            probs = c(.5, .8, .95)
            probs_es = sapply(probs, function(p){
              v<-obj$data$n-obj$data$df_model-1
                     pamlj.glm(u=obj$data$df_effect,v=v,
                                 alpha = obj$data$alpha, power = p,df_model=obj$data$df_model,gpower=obj$options$gncp,tails=obj$tails)$f2
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

      if (obj$tails == "two") 
           alt="two.sided"
      else {
          alt="greater"
      }
                
                .data <- data
                names(.data) <- fields_tothem(obj,names(.data))
                whichnull<-setdiff(c("n","r","sig.level","power"), names(.data))  
                if (length(whichnull)>1)
                   stop("FUNCTION powervector: only one parameters should be NULL")
                if (length(whichnull)==0)
                   stop("FUNCTION powervector: exactly one parameters should be NULL")
                ## for some reason, if n is a vector and the effect size is asked, it gives an error
                .data<-expand.grid(data)  


                results<-lapply(1:nrow(.data),function(i) {
                   one<-.data[i,]
                   pwr::pwr.r.test(n=one$n,r=one$es,power=one$power,sig.level=one$alpha,alternative=alt)
                    })
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 results$es<-results$r
                 odata<-.data[, !names(.data) %in% names(results)]
                 results<-cbind(odata,results)
                 return(results)

  
}

.powervector.glm <- function(obj,data) {
                
                u  <- obj$data$df_effect
                rp <- required_param(data)
                f2 <- NULL
                if (is.something(data$es)) {
                                     data$f2<-obj$toaes(data$es)
                                     data$es<-NULL
                }
                else 
                    rp <- "f2"

                .data<-expand.grid(data)  
                if (is.something(.data$n))
                   .data[["v"]]<- .data$n - obj$data$df_model -1


                 results<-lapply(1:nrow(.data),function(i) {
                   one<-.data[i,]
                   pamlj.glm(u=u,
                             v=one$v,
                             f2=one$f2,
                             power=one$power,
                              alpha=one$alpha,
                              df_model=obj$data$df_model,
                              gpower=obj$options$gncp,
                              tails=obj$tails
                              )
                    
                    })
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 results$es<-obj$fromaes(results$f2)
                 odata<-.data[, !names(.data) %in% names(results)]
                 results<-cbind(odata,results)
                return(results)
}



