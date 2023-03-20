Datamatic <- R6::R6Class(
  "Datamatic",
  cloneable=FALSE,
  class=TRUE,
  inherit = Scaffold,
  public=list(
    vars=NULL,
    variables=NULL,
    dep=NULL,
    labels=NULL,
    initialize=function(jmvobj) {
      
      super$initialize(jmvobj)
      
      self$vars<-unlist(c(self$options$dep,self$options$factors,self$options$covs))
      if (utils::hasName(self$options,"cluster"))
        self$vars<-c(self$options$cluster,self$vars)
      if (utils::hasName(self$options,"offset"))
        self$vars<-c(self$options$offset,self$vars)
      private$.inspect_data(self$analysis$data)
      
    }
  ), ## end of public
  private=list(
     .inspect_data=function(data) {
       
       return()
       
     }    
  ) # end of private
) # end of class
    