

Initer <- R6::R6Class(
  "Initer",
  class=TRUE, 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  inherit = Scaffold,
  public=list(
    dispatcher =  NULL,
    data       =  NULL, ## contains the focus power parameters (input and estimated). It must be one row
    extradata  =  NULL, ## contains (if needed) data required for more complex analysis (multiple effect size, additional coefficient, etc.)
    info       =  list(), ## contains all info regarding the specific analysis. It filled in checkdata and never changed within one analysis
    plots      =  list(), ## contains some specs for customizing plots.
    aim        =  NULL, # the aim of the main analysis (n, power or es)
    caller     =  NULL, # which jamovi analysis is launched (correlation, glm, ttest etc)
    mode       =  NULL, # (if present) the mode within the analysis
    filled     =  TRUE, # some commands need to be filled before we go

    initialize=function(jmvobj) {

      super$initialize(jmvobj)
      ### we want to clean the html message objects
      jmvobj$results$issues$setContent(" ")
      jmvobj$results$plotnotes$setContent(" ")
      jmvobj$results$customnotes$setContent(" ")
      jmvobj$results$extrainfo$setContent(" ")

          self$aim                 <- jmvobj$options$aim
          self$caller              <- jmvobj$options$.caller
          self$mode                <- ifelse(is.null(self$optionValue("mode")),self$caller,self$optionValue("mode"))

          ## fill in the info list with non-changing info about the analysis. Here we set the common
          ## default, in checkdata() are set the specific ones.
          self$info$sig.level      <- jmvobj$options$sig.level
          self$info$power          <- jmvobj$options$power
          
          ## different functions require to transform the effect size to a more suitable effect size ($toaes())
          ## and transform it back to the original scale ($fromaes()). By default there's no 
          ## transformation, for tests that need it, it is set in checkdata() 
      
          self$info$toaes               <- function(value) value
          self$info$fromaes             <- function(value) value
          self$info$loges               <- function(value) FALSE
          self$info$nmin                <- 4
          self$info$nmax                <- 10^7
          self$info$nmax_spell          <- "ten million cases"      
          self$info$nsave               <- 10      
         

          ### some specs for plots
          self$plots$esrange<-3
          self$plots$sensitivity<-TRUE
          
          ## set the class of self so the S3 methods may dispatch to the right functions
          class(self)<-unique(c(self$mode,self$caller,class(self)))
          jinfo("PAMLj: Initializing",self$caller,self$mode)
          ## checkdata update the data depending on the type of test we are running (via S3 dispatch)
          checkdata(self)
          if (self$options$.interface=="jamovi") {
                  jmvobj$results$intro$setContent(info_text(self))   
          }
          else
                  jmvobj$results$intro$setVisible(FALSE) 
          ## some commands produce extra text information, passed here  


  }, # here initialize ends
    #### init functions #####
    init_powertab = function() {
    
        powertab_init(self)
    },
    init_effectsize = function() {
          tab<-effectsize_init(self)
          return(tab)
    },

    init_powerbyes= function() {
      
      list(list(power='\u226450%',desc='Likely miss'),
           list(power='50% \u2013 80%',desc='Good chance of missing'),
           list(power='80% \u2013 95%',desc='Probably detect'),
           list(power='\u226595%',desc='Almost surely detect')
                )
    },      

    init_powerbyn= function() {
      
      list(list(power='\u226450%',desc='Likely miss'),
           list(power='50% \u2013 80%',desc='Good chance of missing'),
           list(power='80% \u2013 95%',desc='Probably detect'),
           list(power='\u226595%',desc='Almost surely detect')
                )
    },      

      init_customtable= function() {
      
       info<-self$analysis$results$powerCustom$state
    
       if (is.null(info))
           return()
       titles<-list(y=stringr::str_to_title(info$y),x=stringr::str_to_title(info$x))
       if (is.something(info$z)) titles$z<-stringr::str_to_title(self$options$plot_z)
       tab<-list(list(y=NA))
       attr(tab,"titles")<-titles
       return(tab)
    } ,
  
    init_means = function() {
      
      effects<-self$info$terms

      tab<-list()
      for (e in effects) {
        .names<-stringr::str_split(e,":")[[1]]
        atab<-rep(NA,length(.names))
        names(atab)<-.names
        ladd(tab)<-list(atab)
      }
        attr(tab,"keys")<-effects
        return(tab)
    }

      
    
  ),   # End public
  
  private=list(

  ) # end of private
) # End Rclass


