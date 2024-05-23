
# This file is a generated template, your changes will not be overwritten

pamlfactorialClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "pamlfactorialClass",
    inherit = pamlfactorialBase,
    private = list(
        .time = NULL,
        .ready= FALSE,
        .smartObjs=list(),
        .plotter=NULL,
        .runner=NULL,
        .init = function() {

                jinfo(paste("MODULE:  PAMLfactorial #### phase init  ####"))
                private$.time<-Sys.time()


     ### set up the R6 workhorse class
                private$.runner          <-  Runner$new(self)

      
      ### info table ###
                 aSmartObj<-SmartTable$new(self$results$powertab,private$.runner)
                 ladd(private$.smartObjs)<-aSmartObj

                 aSmartObj<-SmartTable$new(self$results$powerbyes,private$.runner)
                 ladd(private$.smartObjs)<-aSmartObj

                 aSmartObj<-SmartTable$new(self$results$effectsize,private$.runner)
                 ladd(private$.smartObjs)<-aSmartObj
                 
                 aSmartObj<-SmartArray$new(self$results$means,private$.runner)
                 aSmartObj$expandOnInit<-TRUE
                 aSmartObj$combineBelow="new!"
                 aSmartObj$spaceBy="new!"
                 ladd(private$.smartObjs)<-aSmartObj

          
                 ### init all ####
                 for (tab in private$.smartObjs) {
                     tab$initTable()
                 }
                 ### handle plotter #####
                 private$.plotter<-Plotter$new(self,private$.runner)
                 private$.plotter$initPlots()
      
        }, ## end of init
        .run = function() {
          
                jinfo(paste("MODULE:  PAMLfactorial #### phase run  ####"))
                 private$.runner$run()
                 private$.plotter$preparePlots()

                 for (tab in private$.smartObjs) {
                     tab$runTable()
                 }

        },
        .plot_contour=function(image, ggtheme, theme, ...) {
          
          private$.plotter$plot_contour(image,ggtheme,theme)

          TRUE
          
        },
       .plot_ncurve=function(image, ggtheme, theme, ...) {

          private$.plotter$plot_curve(image,ggtheme,theme)
          return(TRUE)
       },
       .plot_escurve=function(image, ggtheme, theme, ...) {

          private$.plotter$plot_curve(image,ggtheme,theme)
          return(TRUE)
       },
        .plot_custom=function(image, ggtheme, theme, ...) {

          private$.plotter$plot_custom(image,ggtheme,theme)
       }


     
     ) # end of private
) # end of class
