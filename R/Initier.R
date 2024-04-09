

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
    info = list(),
    fromaes=NULL,
    toaes  = NULL,
    logy   = FALSE,
    ok= TRUE,
    nmin=5,
    initialize=function(jmvobj) {

      super$initialize(jmvobj)
      jmvobj$results$issues$setContent(" ")

          ## different functions require to transform the effect size to a more suitable effect size ($toaes())
          ## and transform it back to the original scale ($fromaes()). By default there's no 
          ## transformation, for tests that need it, it is set in checkdata() 
      
          self$toaes               <- function(value) value
          self$fromaes             <- function(value) value

          self$aim                 <- jmvobj$options$aim
          self$data$sig.level      <- jmvobj$options$sig.level
          self$data$power          <- jmvobj$options$power
          self$caller              <- jmvobj$options$.caller
          self$mode                <- ifelse(is.null(self$optionValue("mode")),self$caller,self$optionValue("mode"))

          class(self)<-unique(c(self$caller,self$mode,class(self)))
          
         jinfo("PAMLj: Initializing",self$caller,self$mode)
          ## checkdata update the data depending on the type of test we are running (via S3 dispatch)
          checkdata(self)

          self$data[[self$aim]]  <- NULL  
          self$input             <- self$data

         jmvobj$results$intro$setContent(paste(INFO[["common"]],INFO[[self$mode]]))   


  }, # here initialize ends
    #### init functions #####
    init_powertab = function() {
      
          tab<-list(self$data) 
          attr(tab,"titles")<-list(es=self$data$letter)  
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


