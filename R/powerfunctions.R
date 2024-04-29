### computes the actual power parameters


### computes the  power effect sizes

powerbyes <- function(x, ...) UseMethod(".powerbyes")

.powerbyes.default <- function(obj) {


            probs = c(.5, .8, .95)
            .data<-obj$data
            .data$es<-NULL
            probs_es = sapply(probs, function(p){
              .data$power<-p
               rr<-try_hard(powervector(obj,.data))
               if (isFALSE(rr$error))
                   return(rr$obj$es)
               else
                   return(NA)
           })
         
            emin<-ifelse(is.null(obj$data$esmin),0,obj$data$esmin)
            probs_es<-round(probs_es,digits=3)
            esList <-list(list(es=paste(emin,' <', obj$data$letter, greek_vector["leq"],probs_es[1])),
                          list(es=paste(probs_es[1],'<', obj$data$letter, greek_vector["leq"],probs_es[2])),
                          list(es=paste(probs_es[2],'<', obj$data$letter, greek_vector["leq"],probs_es[3])),
                          list(es=paste(obj$data$letter, greek_vector["geq"],probs_es[3]))
            )

            return(esList)
            
}


.powerbyes.glm <- function(obj) {

            probs = c(.5, .8, .95)
            probs_es = sapply(probs, function(p){
              v<-obj$data$n-obj$data$df_model-1
                     pamlj.glm(u=obj$data$df_effect,v=v,
                                 sig.level = obj$data$sig.level, 
                                 power = p,
                                 df_model=obj$data$df_model,
                                 gpower=obj$options$gncp,
                                 alternative=obj$data$alternative)$f2
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


                .data<-expand.grid(data)
                 names(.data)[names(.data)=="es"]<-"r"
                .names <- intersect(names(.data),rlang::fn_fmls_names(pwr::pwr.r.test))
                .data$alternative<-as.character(.data$alternative)

                results<-lapply(1:nrow(.data),function(i) {
                     one      <-as.list(.data[i,.names])
                     do.call(pwr::pwr.r.test,one)
                    })
                 results<-as.data.frame(do.call("rbind",results))
           
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 odata<-.data[, !names(.data) %in% names(results)]
                 results<-cbind(odata,results)
                 results$n  <- ceiling(results$n)
                 results$es <- results$r
                 results$r  <- NULL
                 return(results)
  
}

.powervector.glm <- function(obj,data) {

                u <- data$df_effect
                
                if (is.something(data$es)) {
                                     data$f2<-obj$toaes(data$es)
                                     data$es<-NULL
                }
                else 
                    data$f2 <- NULL

                if (!is.something(data$n)) 
                                     data$v<-NULL

                .data<-expand.grid(data)  
                if (is.something(.data$n))
                   .data[["v"]]<- .data$n - obj$data$df_model -1
                

                 results<-lapply(1:nrow(.data),function(i) {
                   one<-.data[i,]
                   pamlj.glm(u=u,
                             v=one$v,
                             f2=one$f2,
                             power=one$power,
                              sig.level=one$sig.level,
                              df_model=obj$data$df_model,
                              gpower=obj$options$gncp,
                              alternative=as.character(obj$data$alternative)
                              )
                    
                    })
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 results$es<-obj$fromaes(results$f2)
                 odata<-.data[, !names(.data) %in% names(results)]
                 results<-cbind(odata,results)
                 results$df1<-results$df_effect
                 results$df2<-ceiling(results$v)
             
                return(results)
}


.powervector.ttestind <- function(obj,data) {
                
              
                .data<-expand.grid(data)
                 names(.data)[names(.data)=="es"]<-"d"
                .names <- intersect(names(.data),rlang::fn_fmls_names(pamlj.ttestind))
                .data$alternative<-as.character(.data$alternative)
                if (hasName(.data,"n")) {
                  .data$n1 <- .data$n/(1+.data$n_ratio)
                  .data$n2 <- .data$n1*.data$n_ratio
                }
                results<-lapply(1:nrow(.data),function(i) {
                     one<-as.list(.data[i,.names])
                     do.call(pamlj.ttestind,one)
                    })
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 odata<-.data[, !names(.data) %in% names(results)]
                 results<-cbind(odata,results)
                 results$n  <- results$n1 + results$n2
                 results$df <- results$n - 2
                 results$es <- results$d
                 results$d  <- NULL
                return(results)
}

.powervector.ttestpaired <- function(obj,data) {
                
                .data<-expand.grid(data)
                 names(.data)[names(.data)=="es"]<-"d"
                .names <- intersect(names(.data),rlang::fn_fmls_names(pwr::pwr.t.test))
                .data$alternative<-as.character(.data$alternative)

                results<-lapply(1:nrow(.data),function(i) {
                     one      <-as.list(.data[i,.names])
                     one$type <-as.character(one$type)
                     do.call(pwr::pwr.t.test,one)
                    })
                 results<-as.data.frame(do.call("rbind",results))
                 results$note<-NULL
  
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])

                 odata<-.data[, !names(.data) %in% names(results)]
                 results<-cbind(odata,results)
                 results$n  <- ceiling(results$n)
                 results$df <- results$n - 1
                 results$es <- results$d
                 results$d  <- NULL

                return(results)
}

.powervector.ttestone <- function(obj,data) {
                
  return(.powervector.ttestpaired(obj,data))
}


.powervector.propind <- function(obj,data) {
                
                if (!is.null(data$es)) {
                  data$h<-obj$toaes(data)
                  data$es<-NULL
                } else {
                  data$p1-NULL
                  data$p2<-NULL
                }
                if (is.null(data$n)) {
                  data$n1<-NULL
                  data$n2<-NULL
                } 
                
                .data<-expand.grid(data)

                .names <- intersect(names(.data),rlang::fn_fmls_names(pamlj.propind))
                .data$alternative<-as.character(.data$alternative)
                if (hasName(.data,"n")) {
                  .data$n1 <- .data$n/(1+.data$n_ratio)
                  .data$n2 <- .data$n1*.data$n_ratio
                }
                results<-lapply(1:nrow(.data),function(i) {
                     one<-as.list(.data[i,.names])
                     do.call(pamlj.propind,one)
                    })
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 odata<-.data[, !names(.data) %in% names(results)]
                 results<-cbind(odata,results)
                 results$n  <- results$n1 + results$n2
               
                 tp2 <-   (2 * asin(sqrt(results$p1))) - results$h 
                 results$p2 <- sin(tp2/2)^2
                 results$es <- obj$fromaes(results)
                 results$es[tp2<0]<-NA                  
 

                 results$h  <- NULL
                return(results)
}

.powervector.propone <- function(obj,data) {
                
                if (!is.null(data$es)) {
                  data$h<-obj$toaes(data)
                  data$es<-NULL
                } else {
                  data$p1-NULL
                  data$p2<-NULL
                }
                .data<-expand.grid(data)
                .names <- intersect(names(.data),rlang::fn_fmls_names(pwr::pwr.p.test))
                .data$alternative<-as.character(.data$alternative)
                results<-lapply(1:nrow(.data),function(i) {
                     one<-as.list(.data[i,.names])
                     do.call(pwr::pwr.p.test,one)
                    })
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 odata<-.data[, !names(.data) %in% names(results)]
                 results<-cbind(odata,results)
                 tp2 <-   (2 * asin(sqrt(results$p1))) - results$h 
                 results$p2 <- sin(tp2/2)^2
                 results$es <- obj$fromaes(results)
            
                 results$es[tp2<0]<-NA                  
                 results$n1<-NA
                 results$n2<-NA
                 results$h  <- NULL
                return(results)
}

.powervector.proppaired <- function(obj,data) {
  
  
                if (!is.null(data$es)) {
                  data$psi<-obj$toaes(data)
                  data$es<-NULL
                } 
 
                .data<-expand.grid(data)
                .data$method<-"normal"
                .names <- intersect(names(.data),rlang::fn_fmls_names(pamlj.prop.paired))
                .data$alternative<-as.character(.data$alternative)

                results<-lapply(1:nrow(.data),function(i) {
                     one<-as.list(.data[i,.names])
                     r<-do.call(pamlj.prop.paired,one)
                     r
                    })
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 odata<-.data[, !names(.data) %in% names(results)]
                 results<-cbind(odata,results)
                 results$p2 <- results$p1*results$psi
                 results$es <- obj$fromaes(results)
                 results$n1<-NA
                 results$n2<-NA
                 results$psi  <- NULL
                return(results)
}
