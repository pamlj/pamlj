

Initer <- R6::R6Class(
  "Initer",
  class=TRUE, 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  inherit = Scaffold,
  public=list(
    datavars=NULL,
    ready=FALSE,
    initialize=function(options,dispatcher,data) {

      self$datavars<-names(data)
      super$initialize(options,dispatcher)

    }, # here initialize ends
    #### init functions #####
    
    prepare=function() {

      ok=TRUE
      dep<-trimws(self$options$dep)
      if (!is.something(dep)) {
        name<-"<b>Action required:</b>Please specify a name for the dependent variable"
        self$dispatcher$warnings <-  list(topic="help", message=name)
        ok=FALSE
      } else
        name=dep
      

      if (!(as.numeric(self$options$dep_n)>0)) {
           value<-"<b>Action required:</b> Please specify an `N per cluster`"
           self$dispatcher$warnings <-  list(topic="help", message=value)
           ok=FALSE
      } else
          value=self$options$dep_n
      
      ladd(private$.desc)<-list(var="Dependent",
                      name=name,
                      feature="N per cluster",
                      value=value,
                      structure="Continuous"
          )
      ladd(private$.vars)<-list(
                                role="dependent",  
                                type="numeric",
                                name=name,
                                n=value
      )
      
      
      for (i in seq_along(self$options$factors)) {
             name<-trimws(self$options$factors[[i]]$var)
             if (!is.something(name))
                 next
             
             value<-as.numeric(trimws(self$options$factors[[i]]$opt1))
             
             if (!is.something(value) || is.na(value)) {
                 value<-paste("<b>Action required:</b> Please specify the number of levels for variable <b>",name,"</b>")
                 self$dispatcher$warnings <-  list(topic="help", message=value)
                 ok=FALSE
                 next
             }
             if (value<2) {
               value<-paste("<b>Action required:</b> Please specify more than one level for variable <b>",name,"</b>")
               self$dispatcher$warnings <-  list(topic="help", message=value)
               ok=FALSE
               next
             }
             
             alist<-list(var="nominal",
                         name=name,
                         feature="Levels",
                         value=value,
                         structure="Within")

             blist<- list(
                          role="within",
                          type="nominal",
                          name=name,
                          n=value
             )             
             for (s in self$options$structure)
                 if (name==s$variable) {
                     alist[["structure"]]<-paste("Between across",s$cluster)
                     blist[["role"]]<-c(alist[["role"]],s$cluster)
                 }
                    
             ladd(private$.desc)<-alist
             ladd(private$.vars)<-blist
             
      }
      
      for (i in seq_along(self$options$covs)) {
        
        name<-trimws(self$options$covs[[i]])
        if (!is.something(name))  next
        
        alist<-list(var="Covariate",
                    name=self$options$covs[[i]],
                    feature="",
                    value="",
                    structure="Within")

        blist<- list(
                     role="within",
                     type="numeric",
                     name=name,
                     n=value
        )             
        
        for (s in self$options$structure)
          if (name==s$variable) {
            alist[["structure"]]<-paste("Between across",s$cluster)
            blist[["role"]]<-c(alist[["role"]],s$cluster)
          }
        
        ladd(private$.desc)<-alist
        
        ladd(private$.vars)<-blist
        
      }

      if (!is.something(self$options$clusters)) {
        value<-paste("<b>Action required:</b> Please specify at least on clustersìing variable")
        self$dispatcher$warnings <-  list(topic="help", message=value)
        ok=FALSE
      }
      
      check<-unlist(rlist::list.select(self$options$clusters,var))
      if (is.null((check))) {
        value<-paste("<b>Action required:</b> Please specify at least a clustering variable")
        self$dispatcher$warnings <-  list(topic="help", message=value)
        ok=FALSE
      } else
      for (i in seq_along(self$options$clusters)) {
        name<-trimws(self$options$clusters[[i]]$var)
       
        value<-as.numeric(trimws(self$options$clusters[[i]]$opt1))
        
        if (!is.something(name)) next
        
        if (!is.something(value) || is.na(value)) {
          value<-paste("<b>Action required:</b> Please specify the number of clusters for variable <b>",name,"</b>")
          self$dispatcher$warnings <-  list(topic="help", message=value)
          ok=FALSE
          next
        }
        if (value<2) {
          value<-paste("<b>Action required:</b> Please specify more than one cluster for variable <b>",name,"</b>")
          self$dispatcher$warnings <-  list(topic="help", message=value)
          ok=FALSE
          next
        }
        
        alist<-list(var="Clustering",
                    name=name,
                    feature="N clusters",
                    value=value,
                    structure="")

        blist<-list(
                   role="cluster",
                   type="nominal",
                   name=name,
                   n=value
        )
        ladd(private$.desc)<-alist
        ladd(private$.clusters)<-blist
        
      }
      
      

      self$ready=ok

    },
    init_info=function() {
      private$.desc
    },
    init_clusters=function() {
      
      atable<-as.data.frame(matrix(".",ncol=length(private$.clusters)+1,nrow=5))
      names(atable)<-c(unlist(rlist::list.select(private$.clusters,name)),"Freq")
      return(atable)
      
    },
    init_covs=function() {
      
      
      if (!is.something(self$options$covs))
        atable<-NULL
      else {
        atable<-lapply(self$options$covs, function(cov) list(name=cov))
      }
      
      return(atable)
    },
    
    init_factors=function() {
    
      if (!is.something(self$options$factors))
        return(NULL)
      
      factors<-rlist::list.find(private$.vars,type=="nominal",n=Inf)
      atable<-as.data.frame(matrix(".",ncol=length(factors)+1,nrow=1))
      names(atable)<-c(unlist(rlist::list.select(factors,name)),"Freq")
      
      return(atable)
    }
    
    
    
  ),   # End public
  
  private=list(
    .desc=list(),
    .vars=list(),
    .clusters=list()

  ) # end of private
) # End Rclass


