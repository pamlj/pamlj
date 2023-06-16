GLMRunner <- R6::R6Class("GLMRunner",
                      inherit = Scaffold,
                      cloneable=FALSE,
                      class=TRUE,
                      public=list(
                        model=NULL,
                        etime=0,
                        power_opts=list(),
                        es_list=list(),
                        var_list=list(),
                        tab_info=NULL,
                        datamatic=NULL,
                        initialize = function(jmvobj,datamatic) {
                          
                          jinfo("RUNNER: initializing")
                          super$initialize(jmvobj)
                          self$datamatic<-datamatic
                          es_list<-jmvobj$options$fixed_sizes
                          class(es_list) <- c(paste0("es_type_",jmvobj$options$es),class(es_list))
                          self$es_list   <- es.prepare(es_list,self)
                          self$var_list  <- es.variances(self$es_list,self)
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
                          
                          if (!is.something(self$var_list))
                            return()
                          lapply(self$options$model_terms, function(x) list(source=jmvcore::stringifyTerm(x,raise = T)))
                        },
                        run_anova=function() {
                          
                          if (!is.something(self$var_list))
                            return()
                          results<-private$.fpower(self$var_list)
                          if (!is.something(results))
                              return()
                          results<-lapply(seq_along(results),function(i) {
                            res <- results[[i]]
                            res$eta2 <- res$f2/(1+res$f2)
                            res
                          })
                        
                        #  results<-es.postpare(results)
                          alldf<-attr(self$es_list,"alldf")

                          ladd(results)<-list(source="Residuals",df=alldf,eta=NA,alpha=NA,power=NA,f2=NA,n=NA)
                          minN<-which.max(unlist(rlist::list.map(results, n)))
                          private$.recap<-results[[minN]]
                          private$.recap$test<-"F-test"
                          return(results) 
                           
                        },
                        init_coefficients=function() {
                          
                          if (self$option("es","etap"))
                            return()
                          tab<-lapply(self$es_list, function(x) list(source=jmvcore::stringifyTerm(x$name,raise = T)))
                          return(tab)
                        },
                        run_r2=function() {
                          
                          if (is.something(attr(self$es_list,"r2")))
                             return(list(list(source="R2",value=attr(self$es_list,"r2"),df=attr(self$es_list,"alldf"))))
                          
                        },
                        run_coefficients=function() {
                          
                          if (!is.something(self$es_list))
                            return()
                          
                          results<-private$.fpower(self$es_list)
                          results<-lapply(seq_along(results),function(i) {
                            res <- results[[i]]
                            es  <- self$es_list[[i]]
                            res$beta <-es$beta
                            res$t    <-sqrt(res$f2)*sign(es$original)
                            if (is.na(es$original)) res$original<-0
                            if (!self$option("obtain","es")) res$value<-NA
                            res
                          })
                          #results<-es.postpare(results,self)
                          return(results)
                          
                          minN<-which.max(unlist(rlist::list.map(results, n)))
                          private$.recap<-results[[minN]]
                          private$.recap$es<-round(private$.recap$es,digits = 3)
                          private$.recap$n<-round(private$.recap$n,digits = 0)
                          
                          private$.recap$test<-"t-test"
                          
                          ladd(results)<-list(source="R2",df=alldf,es=r2)
                          
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
                        
                        .fpower=function(es_list) {
                          
                          jinfo("RUNNER: computing F-test power")
                          w<-FALSE
                          results<-list()
                          alldf<-attr(self$es_list,"alldf")
                          for (es in es_list) {
                            f2<-es$f2
                            if (is.na(f2) || f2<.0001) {
                              if (!w) {
                                  warning("Empty parameters reflect effect sizes assumed to be zero.")
                                  w<-TRUE
                              }
                              ladd(results)<-list(n=NA,f2=NA,power=NA,eta2=0,original=0,alpha=NA,df=es$df)
                            } else {
                              n<- NULL
                              if (is.something(self$power_opts$n)) {
                                n<-self$power_opts$n-alldf-1
                                if (n<2)  {
                                  self$warning<-list(topic="info",message=paste("The input sample size (N) is to small to compute power parameters "))
                                  return()
                                }
                              }
                              if (self$option("obtain","es")) f2<-NULL
                              p<-pwr::pwr.f2.test(u=es$df,v=n,f2=f2,power = self$power_opts$power,sig.level = self$power_opts$alpha)
                              n<-round(p$v)+alldf+1
                              ladd(results)<-list(n=n,
                                                  f2=p$f2,
                                                  df=p$u,
                                                  alpha=p$sig.level,
                                                  power=p$power,
                                                  original=es$original)
                            }
                          }
                          return(results)
                        }
                        
                      ) # end of private
) # end of class