
# This file is a generated template, your changes will not be overwritten

pamlmixedClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "pamlmixedClass",
    inherit = pamlmixedBase,
    private = list(
        .time = NULL,
        .ready= FALSE,
        .smartObjs=list(),
        .plotter=NULL,
        .runner=NULL,
        .init = function() {

                jinfo(paste("MODULE:  PAMLmixed #### phase init  ####"))
                private$.time<-Sys.time()
                class(private$.results) <- c('pamlj', class(private$.results)) ## this is useful in R interface


     ### set up the R6 workhorse class
                private$.runner          <-  Runner$new(self)

                 ### handle plotter #####
#                 private$.plotter<-Plotter$new(self,private$.runner)
#                 private$.plotter$initPlots()      
      ### info table ###
                 aSmartObj<-SmartTable$new(self$results$infotab,private$.runner)
                 ladd(private$.smartObjs)<-aSmartObj
                 
                 aSmartObj<-SmartTable$new(self$results$powertab,private$.runner)
                 ladd(private$.smartObjs)<-aSmartObj

                 aSmartObj<-SmartTable$new(self$results$effectsizes,private$.runner)
                 ladd(private$.smartObjs)<-aSmartObj

                 aSmartObj<-SmartTable$new(self$results$showdata,private$.runner)
                 aSmartObj$expandOnInit <- TRUE
                 aSmartObj$expandFrom <- 2
                 aSmartObj$debug <- T
                 ladd(private$.smartObjs)<-aSmartObj
                 
                 ### init all ####
                 for (tab in private$.smartObjs) {
                     tab$initTable()
                 }

      
        }, ## end of init
        .run = function() {
                now <- Sys.time()
                jinfo(paste("MODULE:  PAMLmixed #### phase run  ####"))
                 private$.runner$run()
                 #private$.plotter$preparePlots()
                 for (tab in private$.smartObjs) {
                     tab$runTable()
                 }
                 private$.runner$endrun()
                 
      jinfo("MODULE:  #### phase end ####")

      jinfo("TIME:",Sys.time()-private$.time," secs. Runtime: ",Sys.time()-now)
                 

        },
      .plot_custom=function(image, ggtheme, theme, ...) {

          private$.plotter$plot_custom(image,ggtheme,theme)
       }



     
     ) # end of private
) # end of class
