GLMRunner <- R6::R6Class("GLMRunner",
                      inherit = Scaffold,
                      cloneable=FALSE,
                      class=TRUE,
                      public=list(
                        model=NULL,
                        etime=0,
                        estimate = function(data) {
                          
                          etime<-Sys.time()
                        }, # end of public function estimate
                        init_anova=function() {
                          lapply(self$options$model_terms, function(x) list(source=jmvcore::stringifyTerm(x,raise = T)))
                        },
                        run_anova=function() {
                          
                          k<-length(self$options$model_terms)
                          es<-self$options$fixed_sizes
                          es<-try_hard(lapply(es, function(x) as.numeric(x[[2]])))$obj
                          results<-list()
                          for (e in es) {
                            if (is.na(e)) ladd(results)<-list(n=NA,f2=NA,power=NA,alpha=NA,df=1)
                            else {
                              p<-pwr::pwr.f2.test(u=1,f2=e,power = .80,sig.level = .05)
                              n<-round(p$v)+k
                              ladd(results)<-list(n=n,
                                                  f2=p$f2,
                                                  df=p$u,
                                                  alpha=p$sig.level,
                                                  power=p$power)
                            }
                          }

                          return(results) 
                           
                        }
                        
                      ) ## end of public
) # end of class