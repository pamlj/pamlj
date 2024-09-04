
# This file is a generated template, your changes will not be overwritten

pamlmedClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "pamlmedClass",
    inherit = pamlmedBase,
    private = list(
        .time = NULL,
        .ready= FALSE,
        .smartObjs=list(),
        .plotter=NULL,
        .runner=NULL,
        .init = function() {

                jinfo(paste("MODULE:  PAMLcorr #### phase init  ####"))
                private$.time<-Sys.time()
                class(private$.results) <- c('pamlj', class(private$.results)) ## this is useful in R interface


     ### set up the R6 workhorse class
                private$.runner          <-  Runner$new(self)

                 ### handle plotter #####
                 private$.plotter<-Plotter$new(self,private$.runner)
                 private$.plotter$initPlots()      
      ### info table ###
                 aSmartObj<-SmartTable$new(self$results$powertab,private$.runner)
                 ladd(private$.smartObjs)<-aSmartObj

                 aSmartObj<-SmartTable$new(self$results$effectsize,private$.runner)
                 ladd(private$.smartObjs)<-aSmartObj
                 
                 aSmartObj<-SmartTable$new(self$results$powerbyn,private$.runner)
                 ladd(private$.smartObjs)<-aSmartObj
          
                 aSmartObj<-SmartTable$new(self$results$customtable,private$.runner)
                 aSmartObj$hideOn<-list("z"=NA)
                 ladd(private$.smartObjs)<-aSmartObj
                 
                 ### init all ####
                 for (tab in private$.smartObjs) {
                     tab$initTable()
                 }

      
        }, ## end of init
        .run = function() {
          
                jinfo(paste("MODULE:  PAMLcorr #### phase run  ####"))
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
       },
         .plot_diagram=function(image, ggtheme, theme, ...) {
           
             if (is.null(image$state)) return()
           
             coord<-matrix(c(1,2,1,2,3,3),ncol=2,nrow=3)

             p<-matrix(NA,nrow=2,ncol=3)
             p[2,1]<-1
             p[1,2]<-2
             p[2,3]<-3

             mark(image$state)
             pl<-qgraph::qgraph(coord,
                    edge.color        = "gray",
                    edge.width        = 3, 
                    edge.labels       = image$state$edges,
                    edge.label.cex    = 2.5,
                    edge.label.color  = "black",
                
                    shape             = "rectangle",
                    vsize             = 18,
                    vsize2            = 12,
                    labels            = c("X","M","Y"),
                    layout            = p
               )
             plot(pl)
       }



     
     ) # end of private
) # end of class
