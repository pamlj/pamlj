GLMRunner <- R6::R6Class("GLMRunner",
                      inherit = Scaffold,
                      cloneable=FALSE,
                      class=TRUE,
                      public=list(
                        model=NULL,
                        etime=0,
                        power_opts=list(),
                        tab_info=NULL,
                        datamatic=NULL,
                        init = function() {
                          
                          etime<-Sys.time()
                        }, # end of public function estimate
                        init_info=function() {
                          
                          self$power_opts<-list(alpha=self$options$alpha,
                                                power=self$options$power,
                                                n=self$options$n)
                          
                          rp<-switch (self$options$obtain,
                            n      = { self$power_opts$n<-NULL
                                       c("Required N", "Sample Size")
                                     },
                            alpha  = { self$power_opts$alpha<-NULL
                                       c("Critical alpha","Significance cut-off")
                                     },
                            power  =  { self$power_opts$power<-NULL
                                        c("Obtainable Power","")
                                      },
                            es     =  c("Detectable ES","Smallest ES detactable")
                          )
                          self$tab_info<-list()
                          
                          self$tab_info[['model']] <- list(info="Model",value="OLS linear model",specs="Regression/ANOVA")
                          self$tab_info[['rp']] <- list(info="Requested Parameter",value=rp[1],specs=rp[2])
                          self$tab_info[['alpha']] <- list(info="Alpha",value=self$power_opts$alpha,specs="")
                          self$tab_info[['power']] <- list(info="Power",value=self$poweropts$power,specs="")
                          self$tab_info[['es']] <- list(info="Minumum Effect Size",value="",specs="In the input")
                          self$tab_info[['n']] <- list(info="Required N",value=self$poweropts$n,specs="Overall")
                          self$tab_info[['test']] <- list(info="Inferential Test",value="",specs="Used in estimates")
                               
                        },
                        run_info=function() {
                          
                          recap<-private$.recap
                          self$tab_info[['n']]$value<-recap$n
                          self$tab_info[['es']]$value<-recap$eta
                          self$tab_info[['test']]$value<-recap$test
                          self$tab_info[['power']]$value<-recap$power
                          
                          return(self$tab_info)
                        },
                        init_anova=function() {
                          
                          if (!self$option("es","etap"))
                            return()
                          lapply(self$options$fixed_sizes, function(x) list(source=jmvcore::stringifyTerm(x$name,raise = T)))
                        },
                        run_anova=function() {
                          
                          if (!self$option("es","etap"))
                            return()
                          
                          eslist<-self$options$fixed_sizes
                          eslist<-lapply(eslist,function(x) {
                            x$df<-1
                             vars<-jmvcore::decomposeTerm(gsub("*",":",x,fixed = T))
                             for (var in vars) {
                                  data<-rlist::list.find(self$datamatic$variables,name==var)[[1]]
                                  x$df<-x$df*data$df
                             }
                             x$value<-as.numeric(x$value)/(1-as.numeric(x$value))
                             x
                          })
                          alldf<-sum(unlist(sapply(eslist, function(x) x$df)))
                          results<-private$.fpower(eslist,alldf)
                          
                          if (!is.something(results))
                              return()
                          
                          ladd(results)<-list(source="Residuals",df=alldf,eta=NA,alpha=NA,power=NA,f2=NA,n=NA)
                          minN<-which.max(unlist(rlist::list.map(results, n)))
                          private$.recap<-results[[minN]]
                          private$.recap$test<-"F-test"
                          return(results) 
                           
                        },
                        init_coefficients=function() {
                          
                          if (self$option("es","etap"))
                            return()
                          tab<-lapply(self$options$fixed_sizes, function(x) list(source=jmvcore::stringifyTerm(x$name,raise = T)))
                          ladd(tab)<-list(source="R2")
                          return(tab)
                        },
                        
                        run_coefficients=function() {
                          
                          if (self$option("es","etap"))
                            return()
                          results<-list()
                          eslist<-self$options$fixed_sizes
                          alldf<-length(eslist)
                          eslist<-lapply(eslist, function(x) {
                                         x$value<-as.numeric(x$value)
                                         x$df<-1
                                         return(x)
                                         })
                          r2<-sum(unlist(lapply(eslist, function(x) x$value^2)),na.rm = TRUE)
                          if (r2>1) {
                            self$warning<-list(topic="info",message="The input betas are impossible. Their sum of square should be less than 1.")
                            return()
                          }

                          
                          eslist<-lapply(eslist, function(x) {
                                               x$beta<-x$value
                                               x$value<-x$value^2/(1-r2)
                                               x
                                               })
                          
                          results<-private$.fpower(eslist,alldf)
                          results<-lapply(seq_along(results), function(i) {
                                                     results[[i]]$t<-sqrt(results[[i]]$f2)*sign(eslist[[i]]$value)
                                                     results[[i]]$beta<-sqrt(results[[i]]$f2*(1-r2))*sign(eslist[[i]]$value)
                                                     results[[i]]
                                                     })
                          minN<-which.max(unlist(rlist::list.map(results, n)))
                          private$.recap<-results[[minN]]
                          private$.recap$test<-"t-test"
                          
                          ladd(results)<-list(source="R2",df=alldf,beta=r2)
                          
                          return(results)
                        },
                        recap=function() {
                          text="<div class='jmv-results-table-title-tex'>"
                          text=" nothing yet"
                          text= text %+% "</div>"
                          return(text)
                        }
                      ), ## end of public
                      private=list(
                        .recap=NULL,
                        
                        .fpower=function(eslist,alldf) {
                          
                          w<-FALSE
                          results<-list()
                          for (es in eslist) {
                            esvalue<-try_hard(as.numeric(es$value))$obj
                            if (is.na(esvalue) || abs(esvalue)<.0001) {
                              if (!w) {
                                  warning("Empty parameters reflect effect sizes assumed to be zero.")
                                  w<-TRUE
                              }
                              ladd(results)<-list(n=NA,f2=NA,power=NA,eta=0,alpha=NA,df=es$df)
                            } else {
                              n<- NULL
                              if (is.something(self$power_opts$n)) {
                                n<-self$power_opts$n-alldf
                                if (n<2)  {
                                  self$warning<-list(topic="info",message=paste("The input sample size (N) is to small to compute power parameters "))
                                  return()
                                }
                              }
                              if (esvalue>=1 || esvalue<0) {
                                  self$warning<-list(topic="info",message=paste("The effect size of",es$name," is not compatible with the analysis."))
                                  return()
                              }
                                
                              if (self$option("obtain","es")) esvalue<-NULL
                              p<-pwr::pwr.f2.test(u=es$df,v=n,f2=esvalue,power = self$power_opts$power,sig.level = self$power_opts$alpha)
                              n<-round(p$v)+alldf
                              ladd(results)<-list(n=n,
                                                  f2=p$f2,
                                                  df=p$u,
                                                  eta=p$f2/(1+p$f2),
                                                  alpha=p$sig.level,
                                                  power=p$power)
                            }
                          }
                          return(results)
                        }
                        
                      ) # end of private
) # end of class