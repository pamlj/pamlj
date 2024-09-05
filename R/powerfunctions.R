

## powervector must accept a runner object and a data.frame. It must return a data.frame with nrow() equal to the input data.frame
## they are used across all table and plots to estimate parameters, so the input data.frame is not necessarely the 
## orginal input data of the user.
## Differently to other software, these functions cannot fail. They should return a value (possibly Inf or 0) in any case.
## For this to happen, input data must be checked for plausibility elsewhere (see checkdata() in checkdata.R)

powervector <- function(obj, ...) UseMethod(".powervector")

#do.call(rbind,list(pwr::pwr.r.test(n=10,r=.5)))

.powervector.correlation <- function(obj,data) {

                 aim<-required_param(data)
                 
                 names(data)[names(data)=="es"]<-"r"
                .names <- intersect(names(data),rlang::fn_fmls_names(pwr::pwr.r.test))
                 data$alternative<-ifelse(data$alternative=="two.sided","two.sided","greater")


                results<-lapply(1:nrow(data),function(i) {
                     one      <-as.list(data[i,.names])
                     tryobj<-try_hard(do.call(pwr::pwr.r.test,one), silent=TRUE)
                     out<-tryobj$obj
                     if (!isFALSE(tryobj$error)) {
                       
                     switch(aim,
                            n = {
                                   n<-obj$info$nmin
                                   out<-list(n=n,r=one$r,sig.level=one$sig.level,power=one$power,alternative=one$alternative,method="nmin")
                               },
                            power={ 
                                   out<-list(n=one$n,r=one$r,sig.level=one$sig.level,power=NA,alternative=one$alternative,method="error")
                                  },
                            es={ 
                                   out<-list(n=one$n,r=NA,sig.level=one$sig.level,power=one$power,alternative=one$alternative,method="error")
                                  }
                            
                            )
                     }
                     out
                    })
                          

                 results<-as.data.frame(do.call("rbind",results))
                 if (nrow(results)>3) results<- na.omit(results)
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 
                .names<-c(names(data)[!names( data) %in% names(results)],names(results))
                 odata<- data[, !names( data) %in% names(results)]
                 results<-cbind(odata,results)
                 names(results)<-.names
                 results$n  <- round(results$n,digits=0)
                 results$es <- results$r
                 results$r  <- NULL
                 return(results)
  
}


.powervector.glm <- function(obj,data) {

                aim<-required_param(data)
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
                   tryobj<-try_hard(pamlj.glm(u=one$df_effect,
                             v=one$v,
                             f2=one$f2,
                             power=one$power,
                              sig.level=one$sig.level,
                              df_model=one$df_model,
                              ncp_type=obj$options$ncp_type,
                              alternative=as.character(obj$info$alternative)
                              ), silent=T)
                   out<-tryobj$obj
                 
                   if (!isFALSE(tryobj$error)) {
                     out<-NULL
                     switch(aim,
                            n = {
                               ## if it fails, it means that the required N is smaller than 0, so we report the minimum N
                               n<-obj$info$nmin
                               v <- n- one$df_model -1
                               out<-list(u=one$df_effect,v=v,f2=one$f2,sig.level=one$sig.level,power=one$power,n=n,encp=0,method="nmin")
                               }
                            
                            )
                   }
                    out
                    })
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results)))  results[[i]]<-unlist(results[[i]])
                 odata<-data[, !names(data) %in% names(results)]
                 results<-cbind(odata,results)
                 if (any(results$df_model==1)) obj$info$r2<-results$f2/(1+results$f2)
                 results$es<-obj$info$fromaes(results$f2)
                 
                 results$df1 <-results$df_effect
                 results$df2 <-ceiling(results$v)
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
                 odata<-data[, !(names(data) %in% names(results))]
                 results<-cbind(odata,results)
                 results$df1 <- results$df_effect
                 results$df2 <- ceiling(results$v)
                 results$n   <- round((results$df2/data$edfw)+data$edfb+1)
                 n<-results$n
                 k<- data$edfb+1
                 n = n + k/2;
                 results$bn = (n - (n%%k))/k;
                return(results)
}



#do.call(rbind,list(pamlj.ttestind(n_ratio=1,d=1,power=.90,sig.level=.05)))

.powervector.ttestind <- function(obj,data) {
  
                aim<-required_param(data)
                
                if (is.something(data$es))
                    data$d <- obj$info$toaes(data$es)
                
                if (is.something(obj$info$equi_limit)) {
                    if (is.something(data$power)) data$power<-(1+data$power)/2
                     data$sig.level<- data$sig.level*2
                }
                
                 data$alternative<-as.character(data$alternative)
           
                if (hasName(data,"n")) {
                  data$n1 <- data$n/(1+data$n_ratio)
                  data$n2 <- data$n1*data$n_ratio
                } else {
                    data$n1<-NULL
                    data$n2<-NULL
                }
                .names <- intersect(names(data),rlang::fn_fmls_names(pamlj.ttestind))
                results<-lapply(1:nrow(data),function(i) {
                     one<-as.list(data[i,.names])
                     tryobj<-try_hard(do.call(pamlj.ttestind,one),silent=TRUE)
                     out<-tryobj$obj
                     if (!isFALSE(tryobj$error)) {
                 
                        switch(aim,
                                n = {
                                     n<-obj$info$nmin
                                     n1<-ceiling(n/(1+data$n_ratio))
                                     n2<-n1*one$n_ratio
                                     out<-data.frame(n1=n1,n2=n2,d=one$d,sig.level=one$sig.level,power=one$power,alternative=one$alternative,method="nmin")
                               }
                             )
                     }
                    out
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

#do.call(rbind,list(pwr::pwr.t.test(n=10,d=1)))

.powervector.ttestpaired <- function(obj,data) {
                
                aim<-required_param(data)
                if (is.something(data$es))
                     data$d <- obj$info$toaes(data$es)
                if (is.something(obj$info$equi_limit)) {
                    if (is.something(data$power)) data$power<-(1+data$power)/2
                    data$sig.level <- data$sig.level*2
                }
                .names <- intersect(names(data),rlang::fn_fmls_names(pwr::pwr.t.test))
                 data$alternative<-as.character(data$alternative)
                results<-lapply(1:nrow(data),function(i) {
                     one<-as.list(data[i,.names])
                     one$type <-as.character(one$type)
                     tryobj<-try_hard(do.call(pwr::pwr.t.test,one),silent=TRUE)
                     out<-tryobj$obj
                     if (!isFALSE(tryobj$error)) {
                 
                        switch(aim,
                                n = {
                                     n<-obj$info$nmin
                                     out<-data.frame(n=n,d=one$d,sig.level=one$sig.level,power=one$power,alternative=one$alternative,method="nmin")
                               }
                             )
                     }
                    out
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


#a<-pamlj.propind(n_ratio=1,h=1,sig.level=.05,power=.90)
#do.call(rbind,list(a))
.powervector.propind <- function(obj,data) {
  
                 aim <- required_param(data)

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
                  data$n1[data$n1<2]<-2
                  data$n2 <- data$n1*data$n_ratio
                }
                results<-lapply(1:nrow(data),function(i) {
                     one<-as.list(data[i,.names])
                     tryobj<-try_hard(do.call(pamlj.propind,one),silent=TRUE)
                     res<-tryobj$obj
                     if (!isFALSE(tryobj$error)) {
                       switch(aim,
                              n={
                                 if (round(one$n_ratio*obj$info$nmin) < 2)
                                       n1<-round(obj$info$nmin/one$n_ratio)
                                 else 
                                       n1<-obj$info$nmin/(1+one$n_ratio)
                                res<-list(n1=n1,n2=n1*one$n_ratio,h=one$h,sig.level=one$sig.level,power=one$power,alternative=one$alternative,method="nmin")
                              },
                              es={
                                mark("error in low function for es")
                              }
                              )
                     }
                     res
                    })
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 odata<- data[, !names(data) %in% names(results)]
                 results<-cbind(odata,results)
             
                 results$n  <- results$n1 + results$n2
                 tp2 <-   (2 * asin(sqrt(results$p1))) - results$h 
                 results$p2 <- sin(tp2/2)^2
                 results$es <- obj$info$fromaes(results)
                 
                 results$es[tp2<0]<-obj$info$esmax           
                 results$h  <- NULL
                 
                return(results)
}



#a<-pwr::pwr.p.test(h=1,n=10,sig.level=.05,power=NULL)
#do.call(rbind,list(a))

.powervector.propone <- function(obj,data) {
                
                aim <- required_param(data)
              
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
                     tryobj<-try_hard(do.call(pwr::pwr.p.test,one))
                     out<-tryobj$obj
                     if (!isFALSE(tryobj$error)) {
                       res<-one
                       switch(aim,
                              n={
                                out<-list(h=one$h,n=obj$info$nmin,sig.level=one$sig.level,power=one$power,alternative=one$alternative,method="nmin")
                               },
                              es={
                                mark("error in low function for es")
                              },
                              power={
                                mark("error in low function for power")
                              }

                              )
                     }
                     out

                    })
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 .names<-c(names(data)[!names( data) %in% names(results)],names(results))
                 odata<- data[, !names( data) %in% names(results)]
                 results<-cbind(odata,results)
                 names(results)<-.names

                 tp2 <-   (2 * asin(sqrt(results$p1))) - results$h 
                 results$p2 <- sin(tp2/2)^2
                 results$es <- obj$info$fromaes(results)
                 results$es[tp2<0]<-obj$info$esmax           
                 results$aprox[results$n < obj$info$nmin]<-"nsmall"
                 results$n[results$n < obj$info$nmin] <- obj$info$nmin
                
                 
            
                 results$n1<-NA
                 results$n2<-NA
                 results$h  <- NULL
                return(results)
}


# a<-pamlj.prop.paired(p1=.001,n=6,power=.60,sig.level=.05)
# do.call(rbind,list(a))
# a<-pamlj.prop.paired(p1=.001,psi=732,power=.60,sig.level=.05)
# do.call(rbind,list(a))

.powervector.proppaired <- function(obj,data) {
  
                aim <- required_param(data)
    
                if (!is.null(data$es)) {
                  data$psi<-obj$info$toaes(data)
                  data$es<-NULL
                } 
 
                 data$method<-"normal"
                .names <- intersect(names(data),rlang::fn_fmls_names(pamlj.prop.paired))
                 data$alternative<-as.character(data$alternative)
                results<-lapply(1:nrow(data),function(i) {
                     one<-as.list(data[i,.names])
                     tryobj<-try_hard(do.call(pamlj.prop.paired,one), silent=TRUE)
                     out<-tryobj$obj
                     if (!isFALSE(tryobj$error)) {
                       res<-one
                       switch(aim,
                              n={
                               mark("error in low function for n")
                          
                                out<-list(n=obj$info$nmin,p1=one$p1,psi=one$psi,sig.level=one$sig.level,power=one$power,alternative=one$alternative,method="nmin")
                               },
                              es={
                                mark("error in low function for es")
                                  out<-list(n=one$n,p1=one$p1,psi=NA,sig.level=one$sig.level,power=one$power,alternative=one$alternative,method="eserror")

                              },
                              power={
                                mark("error in low function for power")
                                out<-list(n=one$n,p1=one$p1,psi=one$psi,sig.level=one$sig.level,power=NA,alternative=one$alternative,method="error")

                              }

                              )
                     }
                     out
                    })
                 results<-as.data.frame(do.call("rbind",results))
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                .names<-c(names(data)[!names( data) %in% names(results)],names(results))
                 odata<- data[, !names( data) %in% names(results)]
                 results<-cbind(odata,results)
                 names(results)<-.names

                 results$p2 <- results$p1*results$psi
                 results$es <- obj$info$fromaes(results)
                 results$n1<-NA
                 results$n2<-NA
                 results$psi  <- NULL
                return(results)
}


#### mediation ####

.powervector.mediation <- function(obj,data) {

                 aim<-required_param(data)
                 if (aim=="es") data$a<-NULL
                

                .names <- intersect(names(data),rlang::fn_fmls_names(pamlj.mediation))
               
                results<-lapply(1:nrow(data),function(i) {
                     one      <-as.list(data[i,.names])
                     if (one$test=="mc") fun<-pamlj.mediation.mc
                     else fun<-pamlj.mediation
                     tryobj<-try_hard(do.call(fun,one), silent=F)
                     out<-tryobj$obj
                     if (!isFALSE(tryobj$error)) {
                     switch(aim,
                            n = {
                                stop("failed on n")
                               },
                            power={ 
                                stop("failed on power")
                                  },
                            es={ 
                               stop("failed on es")
                               }
                            
                            )
                     }
                     out
                    })
                          

                 results<-as.data.frame(do.call("rbind",results))
                   mark("med powerfunction res",results)
                 if (nrow(results)>3) results<- na.omit(results)
                 for (i in seq_len(ncol(results))) results[[i]]<-unlist(results[[i]])
                 
                .names<-c(names(data)[!names( data) %in% names(results)],names(results))
                 odata<- data[, !names( data) %in% names(results)]
                 results<-cbind(odata,results)
                 names(results)<-.names
                 results$n  <- round(results$n,digits=0)
                 mark("med powerfunction res",results)

                 return(results)
  
}
