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
                                     tab<-powertab(self)
                                     return(tab)
                                     
                               },
                              run_effectsize = function() {
                                
                                     if (!self$ok) return()
                                     return(effectsize_run(self))
                                    
                               },

                              run_powerbyes = function() {
                                     if (!self$ok) return()
                                     tab <- powerbyes(self)
                                     warning("Estimated for N=",round(self$data$n))
                                     return(tab)
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


