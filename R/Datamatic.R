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
       for (name in names(data)) {
        df<-1
        if (is.factor(data[[name]])) df<-nlevels(data[[name]])-1
        ladd(self$variables)<-list(name=name,df=df) 
       }
     }    
  ) # end of private
) # end of class
    