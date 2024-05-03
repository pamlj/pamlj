## This class takes care of estimating the models and return the results. It inherit from Initer, and defines the same tables
## defined by Initer, but it fills them with the results. It also adds a few tables not defined in Initer
## Any function that produce a table goes here

Runner <- R6::R6Class("Runner",
                        inherit = Initer,
                        cloneable=FALSE,
                        class=TRUE,
                        public=list(
                              run= function() {
                                 checkdata(self)
                                 self$data[[self$aim]]  <- NULL  
                                 self$input             <- self$data
                                
                                resobj <- try_hard(powervector(self,self$input) )
                                if (!isFALSE(resobj$warning))
                                     warning(resobj$warning)
                                if (!isFALSE(resobj$error)) {
                                            checkfailure(self,resobj)
                                            self$ok <- FALSE
                                            return(NULL)
                                }
                                self$data<-as.list(resobj$obj)
                             
                            
                              },
                              run_powertab = function() {
                                     if (!self$ok) return()
                                     l<-list(self$data)
                                     return(l)
                               },
                              run_effectsize = function() {
                                
                                     return(effectsize_run(self))
                                    
                               },

                              run_powerbyes = function() {

                                     if (!self$ok) return()
                                     results <- powerbyes(self)
                                     warning("Estimated for N=",round(self$data$n))
                                     return(results)
                               },
                              run_customtable = function() {
     
                                     if (!self$ok) return()
                                     info <- self$analysis$results$powerCustom$state
                                     if (is.null(info))
                                         return()
                                     

                                     return(info$data)
                               }




                          ), # end of public function estimate

                        private=list(
                          # do private stuff
                          
                        ) #end of private
)  # end of class


