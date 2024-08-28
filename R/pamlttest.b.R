
# This file is a generated template, your changes will not be overwritten

pamlttestClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "pamlttestClass",
    inherit = pamlttestBase,
    private = list(
        .time = NULL,
        .ready= FALSE,
        .smartObjs=list(),
        .plotter=NULL,
        .runner=NULL,
        .init = function() {

                jinfo(paste("MODULE:  PAMLttest #### phase init  ####"))
                private$.time<-Sys.time()
                class(private$.results) <- c('pamlj', class(private$.results)) ## this is useful in R interface


     ### set up the R6 workhorse class
                private$.runner          <-  Runner$new(self)

                 ### handle plotter #####
                 ### plotter$initPlots() should come before initing customtable, because
                 ##  customtable relies on the computation done for custom plot
                 private$.plotter<-Plotter$new(self,private$.runner)
                 private$.plotter$initPlots()

      
      ### info table ###
                 aSmartObj<-SmartTable$new(self$results$powertab,private$.runner)
                 aSmartObj$hideOn<-list("z"=NA, "n1"=NA, "n2"=NA)
                 ladd(private$.smartObjs)<-aSmartObj

                 aSmartObj<-SmartTable$new(self$results$powerbyes,private$.runner)
                 ladd(private$.smartObjs)<-aSmartObj
 
                 aSmartObj<-SmartTable$new(self$results$customtable,private$.runner)
                 aSmartObj$hideOn<-list("z"=NA, "n1"=NA, "n2"=NA)
                 ladd(private$.smartObjs)<-aSmartObj
          
                 ### init all ####
                 for (tab in private$.smartObjs) {
                     tab$initTable()
                 }
 
      
        }, ## end of init
        .run = function() {
          
                jinfo(paste("MODULE:  PAMLttest #### phase run  ####"))
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
