## This class takes care of estimating the models and return the results. It inherit from Initer, and defines the same tables
## defined by Initer, but it fills them with the results. It also adds a few tables not defined in Initer
## Any function that produce a table goes here

Runner <- R6::R6Class("Runner",
                        inherit = Initer,
                        cloneable=FALSE,
                        class=TRUE,
                        public=list(
                          model=NULL,
                          initialize=function(options,dispatcher,data) {
                            super$initialize(options,dispatcher,data)
                          },
                          run = function() {
                            
                            if (!self$option("output"))
                               return()
                            
                            type<-self$options$clusters_comb
                            private$.data<-.asample(private$.vars,private$.clusters,type = type)
                          },
                          savedata=function(results) {
                            


                            if (self$option("output") && results$output$isNotFilled()) {

                                results$output$set(1:ncol(private$.data),
                                                 names(private$.data),
                                                 rep("var",ncol(private$.data)),
                                                 rep("continuous",ncol(private$.data)))
                                results$output$setValues(private$.data)
                            }                    
                          }

                          ), # end of public function estimate

                        private=list(
                          .data=data.frame()
                          # do private stuff
                        ) #end of private
)  # end of class


