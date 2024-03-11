## This class takes care of estimating the models and return the results. It inherit from Initer, and defines the same tables
## defined by Initer, but it fills them with the results. It also adds a few tables not defined in Initer
## Any function that produce a table goes here

Runner <- R6::R6Class("Runner",
                        inherit = Initer,
                        cloneable=FALSE,
                        class=TRUE,
                        public=list(
                          
                              run_powertab = function() {
      
                                     results <- powerfunction(self)
                                     mark(results)
                                     return(list(results))
                               },
                              run_powerbyes = function() {
      
                                     results <- powerbyes(self)
                                     return(results)
                               }

                          ), # end of public function estimate

                        private=list(
                          # do private stuff
                          
                        ) #end of private
)  # end of class


