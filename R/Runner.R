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
                            
                            type<-self$options$clusters_comb
                            private$.data<-.asample(private$.vars,private$.clusters,type = type)
                          },
                          savedata=function(results) {
                            

                            if (self$option("output") && results$output$isNotFilled()) {
                                
                                .namesvars<-unlist(rlist::list.select(private$.vars,name))
                                .namesclusters<-unlist(rlist::list.select(private$.clusters,name))
                                .names<-c(.namesvars,.namesclusters)
                                
                                .typevars<-unlist(rlist::list.select(private$.vars,type))
                                .typeclusters<-rep("nominal",length(.namesclusters))
                                .types<-c(.typevars,.typeclusters)
                               

                                data<-private$.data[,.names]
                                k<-length(.names)

                                results$output$set(1:k,
                                                 .names,
                                                 rep("var",k),
                                                 .types
                                )
                                results$output$setValues(data)
                            }                    
                          },
                          
                          run_clusters=function() {
                            
                            .n <- prod(unlist(rlist::list.select(private$.clusters,n)))
                            .names <- unlist(rlist::list.select(private$.clusters,name))
                            abridged<-FALSE
                            if (length(.names)>1 && .n>25) {
                              .n<-5
                              abridged=TRUE
                            }
                            
                            data<-subset(private$.data,select=.names)
                            
                            if (self$option("clusters_comb","cross")) {

                            for (cluster in .names) {
                              levs<-levels(data[[cluster]])
                              levs<-levs[1:min(length(levs),.n)]
                              data<-subset(data,data[[cluster]] %in% levs)
                              data[[cluster]]<-factor(data[[cluster]])
                            }
                            } else {
                              cluster<-.names[[1]]
                              levs<-levels(data[[cluster]])
                              levs<-levs[1:min(length(levs),.n)]
                              data<-subset(data,data[[cluster]] %in% levs)
                              data[[cluster]]<-factor(data[[cluster]])
                              
                            }
                              
                            atable <- table(data)
                            atable <- as.data.frame(atable,stringsAsFactors = FALSE)
                            names(atable)[1:length(.names)]<-.names
                            atable<-subset(atable,Freq>0)
                            if (abridged)
                              self$dispatcher$warnings<-list(topic="clusters",message=paste("Only the first",.n,"levels are shown"))
                            
                            return(atable)
                          },
                          run_covs=function() {
                            
                            covs<-rlist::list.find(private$.vars, type=="numeric",n=Inf)
                            alist<-list()
                            data<-private$.data
                            for (cov in covs) {
                              type=cov$type
                              for (cluster in private$.clusters) {
                                 sd_between<-unlist(tapply(data[[cov$name]],list(data[[cluster$name]]),mean))
                                 sd_between<-sd(sd_between)
                                 sd_within<-unlist(tapply(data[[cov$name]],list(data[[cluster$name]]),sd))
                                 sd_within<-mean(sd_within)
                                 
                                 ladd(alist)<-list(name=cov$name,
                                                mean=mean(private$.data[[cov$name]]),
                                                sd=sd(private$.data[[cov$name]]),
                                                cluster=cluster$name,
                                                sd_between=sd_between,
                                                sd_within=sd_within,
                                                type=cov$type)
                              }
                            }

                            return(alist)
                          },
                          run_factors=function() {
                            
                            if (!is.something(self$options$factors))
                              return(NULL)
                            
                            factors <- rlist::list.find(private$.vars,type=="nominal",n=Inf)
                            .names  <- unlist(rlist::list.select(factors,name))
                            data<-subset(private$.data,select=.names)
                            atable <- table(data)
                            atable <- as.data.frame(atable,stringsAsFactors = FALSE)
                            names(atable)[1:length(.names)]<-.names
                            atable<-subset(atable,Freq>0)
                            return(atable)
                          }
                          

                          ), # end of public function estimate

                        private=list(
                          .data=data.frame()
                          # do private stuff
                        ) #end of private
)  # end of class


