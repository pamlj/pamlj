

Initer <- R6::R6Class(
  "Initer",
  class=TRUE, 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  inherit = Scaffold,
  public=list(
    datavars=NULL,
    dispatcher=NULL,
    ready=FALSE,
    data = list(),
    aim  = NULL,
    caller=NULL,
    tails = NULL,
    initialize=function(jmvobj) {

      super$initialize(jmvobj)
          self$aim               <- jmvobj$options$aim
          self$data[["es"]]      <- jmvobj$options$es
          self$data[["n"]]       <- jmvobj$options$sample
          self$data[["alpha"]]   <- jmvobj$options$alpha
          self$data[["power"]]   <- jmvobj$options$power
          self$data[[self$aim]]  <- NULL
          self$tails             <- jmvobj$options$tails
          self$caller            <- jmvobj$options$.caller
          class(self)<-c(class(self),jmvobj$options$.caller)

    }, # here initialize ends
    #### init functions #####
    init_powertab = function() {
      
          tab<-list(self$data) 
          
          return(tab)
    },
    init_powerbyes= function() {
      
      list(list(power='\u226450%',desc='Likely miss'),
           list(power='50% \u2013 80%',desc='Good chance of missing'),
           list(power='80% \u2013 95%',desc='Probably detect'),
           list(power='\u226595%',desc='Almost surely detect')
                )
      
    }      
      
    
  ),   # End public
  
  private=list(
    .desc=list(),
    .vars=list(),
    .clusters=list()

  ) # end of private
) # End Rclass


