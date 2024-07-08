## This class takes care of estimating the models and return the results. It inherit from Initer, and defines the same tables
## defined by Initer, but it fills them with the results. It also adds a few tables not defined in Initer
## Any function that produce a table goes here

Runner <- R6::R6Class("Runner",
                        inherit = Initer,
                        cloneable=FALSE,
                        class=TRUE,
                        public=list(
                              run= function() {
                                
                                 # this is run before any table or plot is filled.
                                 # it produces the basic estimation required for all tables and plots
                                 # it fills self$data with all power parameters
                                 checkdata(self)
                               
                                 jinfo("PAMLj: Runner: first estimation")
                                 if (!self$ok) return()
                                 resobj <- try_hard(powervector(self,self$data) )
                                 if (!isFALSE(resobj$warning))
                                     warning(resobj$warning)
                                 if (!isFALSE(resobj$error)) {
                                            checkfailure(self,resobj)
                                            self$ok <- FALSE
                                            return(NULL)
                                 }
                                 # everything went well, so fill self$data
                                 self$data<-resobj$obj
                                
                              },
                              run_powertab = function() {
                                     if (!self$ok) return()
                                     jinfo("PAMLj: Runner: powertab")
                                     tab<-powertab(self)
                                     return(tab)
                                     
                               },
                              run_effectsize = function() {
                                
                                     if (!self$ok) return()
                                     return(effectsize_run(self))
                                    
                               },

                              run_powerbyes = function() {
                                     if (!self$ok) return()
                                     jinfo("PAMLj: Runner: powerbyes")
                                
                                     tab <- powerbyes(self)
                                     warning("Estimated for N=",round(self$data$n))
                                     return(tab)
                               },
                              run_means = function() {
                          
                                  exdata<-self$analysis$data
                                  factors <- self$options$factors
                                  means   <- self$options$means
                                  sds     <- self$options$sds
                                  if (is.null(means))   return()
                                  if (is.null(sds))     return()
                                  if (is.null(factors)) return()


                                  for (f in factors) {
                                        exdata[[f]]<-factor(exdata[[f]])
                                        contrasts(exdata[[f]])<-contr.sum(nlevels(exdata[[f]]))
                                  }
                                  form1<-paste(means,"~",paste(factors,collapse="*"))
                                  model1<-lm(form1,exdata)
                                  form2<-paste(sds,"~",paste(factors,collapse="*"))
                                  model2<-lm(form2,exdata)

                                  effects<-self$extradata$effect
                                  suppressWarnings({
                                  tabs<-lapply(effects,function(e) {
                                    form<-as.formula(paste("~",e))
                                    em<-as.data.frame(emmeans::emmeans(model1,specs=form))
                                    es<-as.data.frame(emmeans::emmeans(model2,specs=form))
                                    for (n in names(em)) if (is.factor(em[[n]])) em[[n]]<-as.character(em[[n]])
                                    w<-which(names(em)=="emmean")
                                    em<-em[,1:w]
                                    em$sd<-es$emmean
                                    for (i in w:1) em<-em[order(em[[i]]),]
                                    em
                                  })
                                  })
                                  return(tabs)
                                
                              },
                              run_customtable = function() {
     
                                     if (!self$ok) return()
                                     ## this is filled by plotter$prepateCustom
                                     ## here we simply pass it to the table
                                     state <- self$analysis$results$powerCustom$state
                                     if (is.null(state))
                                         return()
                                     return(state$data)
                               }

                          ), # end of public function estimate

                        private=list(
                          # do private stuff
                          
                        ) #end of private
)  # end of class


