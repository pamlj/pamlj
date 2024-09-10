
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
                 
                 aSmartObj<-SmartTable$new(self$results$powerxy,private$.runner)
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
                 private$.checkpoint()
                 for (tab in private$.smartObjs) {
                     tab$runTable()
                 }
      jinfo("MODULE:  #### phase end ####")

      jinfo("TIME:",Sys.time()-private$.time," secs")
                 

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
             
            m<-image$state$enlarge

             pl<-qgraph::qgraph(image$state$coord,
                edge.color        = "gray",
                edge.width        = 3 * m, 
                edge.label.cex    = 1.6 * m,
                edge.label.color  = "black",
                edge.labels       = image$state$edge.labels ,
                edge.label.margin = .05,
                edge.label.position = image$state$pos,
                shape             = "rectangle",
                labels            =  image$state$labels,
                label.cex         =  .7,
                vsize             = 18 * m,
                vsize2            = 10 * m,
                layout            = image$state$p,
                curve             = image$state$curve,
                bidirectional     = T
               
               )
             plot(pl)
       }



     
     ) # end of private
) # end of class
