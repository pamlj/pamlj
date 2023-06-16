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
    makedata=function() {
      
      lapply(self$variables, function(x) 1:x$nlev)
      sdata<-as.data.frame(do.call(expand.grid,list(a,b,d)))
      names(sdata)<-c("cat3","cat2","x")
      sdata$cat3<-factor(sdata$cat3)
      sdata$cat2<-factor(sdata$cat2)
      contrasts(sdata$cat3)<-contrasts(sdata$cat3)-(1/3)
      contrasts(sdata$cat2)<-contrasts(sdata$cat2)-(1/2)
      
    }
  ), ## end of public
  private=list(
     .inspect_data=function(data) {
       for (name in names(data)) {
        df<-1
        levs<-1
        nlevs<-1
        type="numeric"
        if (is.factor(data[[name]])) {
           type="factor"
           lves<-levels(data[[name]])
           nlevs<-length(levs)
           df<-nlevs-1
        }
        ladd(self$variables)<-list(name=name,type=type, df=df,levs=levs,nlevs=nlevs) 
       }
     }    
  ) # end of private
) # end of class
    