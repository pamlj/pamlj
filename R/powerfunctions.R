

## powervector must accept a runner object and a data.frame. It must return a data.frame with nrow() equal to the input data.frame
## they are used across all table and plots to estimate parameters, so the input data.frame is not necessarely the 
## orginal input data of the user.

powervector <- function(obj, ...) UseMethod(".powervector")


.powervector.correlation <- function(obj,data) {


                 names(data)[names(data)=="es"]<-"r"
                .names <- intersect(names(data),rlang::fn_fmls_names(pwr::pwr.r.test))
                 data$alternative<-ifelse(data$alternative=="two.sided","two.sided","greater")


                results<-lapply(1:nrow(data),function(i) {
                     one      <-as.list(data[i,.names])
                     do.call(pwr::pwr.r.test,one)
                    })
                 results<-as.data.frame(do.call("rbind",results))
           
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 odata<- data[, !names(data) %in% names(results)]
                 results<-cbind(odata,results)
                 results$n  <- ceiling(results$n)
                 results$es <- results$r
                 results$r  <- NULL
                 return(results)
  
}

.powervector.glm <- function(obj,data) {


                if (is.something(data$es)) {
                                     data$f2<-obj$info$toaes(data$es)
                                     data$es<-NULL
                }
                else 
                    data$f2 <- NULL

                if (!is.something(data$n)) 
                                     data$v<-NULL
                else
                    data[["v"]]<- data$n - data$df_model -1

                 results<-lapply(1:nrow(data),function(i) {
                   one<-data[i,]
                   mark(one,is.something(data$n),data$n)
                   pamlj.glm(u=one$df_effect,
                             v=one$v,
                             f2=one$f2,
                             power=one$power,
                              sig.level=one$sig.level,
                              df_model=one$df_model,
                              ncp_type=obj$options$ncp_type,
                              alternative=as.character(obj$info$alternative)
                              )
                    
                    })
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 results$es<-obj$info$fromaes(results$f2)
                 odata<-data[, !names(data) %in% names(results)]
                 results<-cbind(odata,results)
                 results$df1<-results$df_effect
                 results$df2<-ceiling(results$v)
               
                return(results)
}


.powervector.factorial <- function(obj,data) {

                jinfo("PAMLj: Factorial power function")
                if (is.something(data$es)) {
                                     data$f2<-obj$info$toaes(data$es)
                                     data$es<-NULL
                }
                else 
                    data$f2 <- NULL
                if (!is.something(data$n)) 
                                     data$v<-NULL
                else
                    data[["v"]]<- data$edfw*(data$n-data$edfb-1)


                 results<-lapply(1:nrow(data),function(i) {
                   one<-data[i,]
                   if (one$type=="w" && obj$options$ncp_type=="gpower") ncp<-"strict"
                   else ncp<-obj$options$ncp_type

                   pamlj.glm(u=one$df_effect,
                             v=one$v,
                             f2=one$f2,
                             power=one$power,
                              sig.level=one$sig.level,
                              df_model=one$df_model,
                              ncp_type=ncp,
                              alternative=as.character(obj$info$alternative)
                              )
                    
                    })
  
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 results$es<-obj$info$fromaes(results$f2)
                 odata<-data[, !names(data) %in% names(results)]
                 results<-cbind(odata,results)
                 results$df1 <- results$df_effect
                 results$df2 <- ceiling(results$v)
                 results$n   <- round((results$df2/data$edfw)+data$edfb+1)
                 n<-results$n
                 k<- data$edfb+1
                 n = n + k/2;
                 results$nb = (n - (n%%k))/k;
                return(results)
}




.powervector.ttestind <- function(obj,data) {
  
                if (is.something(data$es))
                    data$d <- obj$info$toaes(data$es)
                if (is.something(obj$info$equi_limit)) {
                    if (is.something(data$power)) data$power<-(1+data$power)/2
                     data$sig.level<- data$sig.level*2
                }
                 .names <- intersect(names(data),rlang::fn_fmls_names(pamlj.ttestind))
                 data$alternative<-as.character(data$alternative)
                if (hasName(data,"n")) {
                  data$n1 <- data$n/(1+data$n_ratio)
                  data$n2 <- data$n1*data$n_ratio
                }

                results<-lapply(1:nrow(data),function(i) {
                     one<-as.list(data[i,.names])
                     do.call(pamlj.ttestind,one)
                    })
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 odata<-data[, !names(data) %in% names(results)]
                 results<-cbind(odata,results)
                 results$n  <- results$n1 + results$n2
                 results$df <- results$n - 2
                 results$es <- obj$info$fromaes(results$d)
                 if (is.something(obj$data$equi_limit)) {
                    results$power<-2*results$power-1
                    results$sig.level<-results$sig.level/2
                 }
                 results$power[results$power<0] <- NA
                 results$d  <- NULL
                return(results)
}

.powervector.ttestpaired <- function(obj,data) {
                
                if (is.something(data$es))
                     data$d <- obj$info$toaes(data$es)
                if (is.something(obj$info$equi_limit)) {
                    if (is.something(data$power)) data$power<-(1+data$power)/2
                    data$sig.level <- data$sig.level*2
                }
                .names <- intersect(names(data),rlang::fn_fmls_names(pwr::pwr.t.test))
                 data$alternative<-as.character(data$alternative)
                results<-lapply(1:nrow(data),function(i) {
                     one      <-as.list(data[i,.names])
                     one$type <-as.character(one$type)
                     do.call(pwr::pwr.t.test,one)
                    })
                 results<-as.data.frame(do.call("rbind",results))
                 results$note<-NULL

                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])

                 odata<-subset(data, select= !(names(data) %in% names(results)))
                 results<-cbind(odata,results)
                 results$n  <- ceiling(results$n)
                 results$df <- results$n - 1
                 results$es <- obj$info$fromaes(results$d)
                 if (is.something(obj$info$equi_limit)) {
                    results$power<-2*results$power-1
                    results$sig.level<-results$sig.level/2
                 }
                 results$power[results$power<0] <- NA
                 results$d  <- NULL

                return(results)
}

.powervector.ttestone <- function(obj,data) {
                
  return(.powervector.ttestpaired(obj,data))
}


.powervector.propind <- function(obj,data) {
                
                if (!is.null(data$es)) {
                  data$h<-obj$info$toaes(data)
                  data$es<-NULL
                } else {
                  data$p1-NULL
                  data$p2<-NULL
                }
                if (is.null(data$n)) {
                  data$n1<-NULL
                  data$n2<-NULL
                } 
               .names <- intersect(names(data),rlang::fn_fmls_names(pamlj.propind))
                data$alternative<-as.character(data$alternative)
                if (hasName(data,"n")) {
                  data$n1 <- data$n/(1+data$n_ratio)
                  data$n2 <- data$n1*data$n_ratio
                }
                results<-lapply(1:nrow(data),function(i) {
                     one<-as.list(data[i,.names])
                     do.call(pamlj.propind,one)
                    })
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 odata<- data[, !names(data) %in% names(results)]
                 results<-cbind(odata,results)
                 results$n  <- results$n1 + results$n2
               
                 tp2 <-   (2 * asin(sqrt(results$p1))) - results$h 
                 results$p2 <- sin(tp2/2)^2
                 results$es <- obj$info$fromaes(results)
                 results$es[tp2<0]<-NA                  
                 results$h  <- NULL
                return(results)
}

.powervector.propone <- function(obj,data) {
                
                if (!is.null(data$es)) {
                  data$h<-obj$info$toaes(data)
                  data$es<-NULL
                } else {
                  data$p1-NULL
                  data$p2<-NULL
                }
                .names <- intersect(names(data),rlang::fn_fmls_names(pwr::pwr.p.test))
                 data$alternative<-as.character(data$alternative)
                 results<-lapply(1:nrow(data),function(i) {
                     one<-as.list(data[i,.names])
                     do.call(pwr::pwr.p.test,one)
                    })
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 odata<- data[, !names( data) %in% names(results)]
                 results<-cbind(odata,results)
                 tp2 <-   (2 * asin(sqrt(results$p1))) - results$h 
                 results$p2 <- sin(tp2/2)^2
                 results$es <- obj$info$fromaes(results)
            
                 results$es[tp2<0]<-NA                  
                 results$n1<-NA
                 results$n2<-NA
                 results$h  <- NULL
                return(results)
}

.powervector.proppaired <- function(obj,data) {
  
  
                if (!is.null(data$es)) {
                  data$psi<-obj$info$toaes(data)
                  data$es<-NULL
                } 
 
                 data$method<-"normal"
                .names <- intersect(names(data),rlang::fn_fmls_names(pamlj.prop.paired))
                 data$alternative<-as.character(data$alternative)

                results<-lapply(1:nrow(data),function(i) {
                     one<-as.list(data[i,.names])
                     r<-do.call(pamlj.prop.paired,one)
                     r
                    })
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 odata<- data[, !names( data) %in% names(results)]
                 results<-cbind(odata,results)
                 results$p2 <- results$p1*results$psi
                 results$es <- obj$info$fromaes(results)
                 results$n1<-NA
                 results$n2<-NA
                 results$psi  <- NULL
                return(results)
}
