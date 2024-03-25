

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
    input=list(),
    aim  = NULL,
    caller=NULL,
    mode = NULL,
    tails = NULL,
    info = list(),
    fromaes=NULL,
    toaes  =NULL,
    ok= TRUE,
    nmin=5,
    initialize=function(jmvobj) {

      super$initialize(jmvobj)
          self$toaes               <- function(value) value
          self$fromaes             <- function(value) value

          self$aim               <- jmvobj$options$aim
          self$data[["n"]]       <- jmvobj$options$sample
          self$data[["alpha"]]   <- jmvobj$options$alpha
          self$data[["power"]]   <- jmvobj$options$power
          self$tails             <- jmvobj$options$tails
          self$caller            <- jmvobj$options$.caller
          
          if (self$option("mode")) {
                 self$mode <- self$options$mode
                if (self$mode == "beta") {
                      self$data[["es"]]         <- as.numeric(jmvobj$options$b_es)
                      self$data["df_model"]     <- jmvobj$options$b_df_model
                      self$data["df_effect"]    <- 1
                      self$data["r2"]           <- jmvobj$options$b_r2
                      self$toaes                <- function(value)  value^2/(1-self$data$r2)
                      self$fromaes              <- function(value)  sqrt(value*(1-self$data$r2)) 
                }
                if (self$mode == "peta") {
                    self$data[["es"]]         <- as.numeric(jmvobj$options$v_es)
                    self$data["df_model"]     <- jmvobj$options$v_df_model
                    self$data["df_effect"]    <- jmvobj$options$v_df_effect
                    self$toaes                <- function(value) value/(1-value)
                    self$fromaes              <- function(value) value/(1+value)
                }
                if (self$mode == "eta") {
                    self$data[["es"]]         <- as.numeric(jmvobj$options$e_es)
                    self$data["df_model"]     <- jmvobj$options$e_df_model
                    self$data["df_effect"]    <- jmvobj$options$e_df_effect
                    self$data["r2"]           <- jmvobj$options$e_r2
                    self$toaes                <- function(value)  value/(1-self$data$r2)
                    self$fromaes              <- function(value)  value*(1-self$data$r2) 
                }
                self$nmin <- self$data$df_model+3

          } #end of mode selection
          
           if (is.null(self$mode)) self$mode<-self$caller
           jmvobj$results$intro$setContent(paste(INFO[["common"]],INFO[[self$mode]]))   
           if (!is.something(self$data$es))
                self$data$es <- self$options$es

          self$data[[self$aim]]  <- NULL  

          class(self)<-c(self$caller,self$mode,class(self))
          jmvobj$results$issues$setContent(" ")

          checkdata(self)
          self$input             <- self$data


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
      
    },      

    init_powerr2_powertab = function() {
      
          tab<-list(self$data) 
          return(tab)
    },

    init_powerr2_powerbyes= function() {
      
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


