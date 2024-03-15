## This class takes care of estimating the models and return the results. It inherit from Initer, and defines the same tables
## defined by Initer, but it fills them with the results. It also adds a few tables not defined in Initer
## Any function that produce a table goes here

Runner <- R6::R6Class("Runner",
                        inherit = Initer,
                        cloneable=FALSE,
                        class=TRUE,
                        public=list(
                              run= function() {
                                powerfunction(self)
                              },
                              run_powertab = function() {
      
                                     return(list(self$data))
                               },
                              run_powerbyes = function() {
      
                                     results <- powerbyes(self)
                                     warning("Estimated for N=",self$data$n)
                                     return(results)
                               },
                              run_powerr2_powertab = function() {
                                
                                     data<-self$input
                                     whichnull<-required_param(data)
                                     data$es<-data$r2/(1-data$r2)
                                     data$u<-data$df_model
                                     value<-powervector(self,data) 
                                     data[[whichnull]]<-value
                                     data$df1<-data$df_model
                                     data$df2<-data$n-data$df_model-1
                                     return(list(data))
                                     
                               },
                                run_powerr2_powerbyes = function() {
                                  
                                     tab<-powerbyr2(self)
                                     warning("Estimated for N=",self$data$n)
                                    return(tab)

                               }



                          ), # end of public function estimate

                        private=list(
                          # do private stuff
                          
                        ) #end of private
)  # end of class


