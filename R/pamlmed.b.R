
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

                jinfo(paste("MODULE:  PAMLmed #### phase init  ####"))
                private$.time<-Sys.time()
                class(private$.results) <- c('pamlj', class(private$.results)) ## this is useful in R interface


     ### set up the R6 workhorse class
                private$.runner          <-  Runner$new(self)

                 ### handle plotter #####
                 private$.plotter<-Plotter$new(self,private$.runner)
                 private$.plotter$initPlots()      
      ### info table ###
                 aSmartObj<-SmartTable$new(self$results$powertab,private$.runner)
                 ## Do not pre-fill the main mediation table during init; the
                 ## run phase must write the solved N/power/effect result.
                 aSmartObj$initSource <- NULL
                 ladd(private$.smartObjs)<-aSmartObj

                 aSmartObj<-SmartTable$new(self$results$effectsize,private$.runner)
                 ladd(private$.smartObjs)<-aSmartObj
                 
                 aSmartObj<-SmartTable$new(self$results$powerbyn,private$.runner)
                 ladd(private$.smartObjs)<-aSmartObj

                 aSmartObj<-SmartTable$new(self$results$powerbyes,private$.runner)
                 ladd(private$.smartObjs)<-aSmartObj
                 
                 aSmartObj<-SmartTable$new(self$results$powerxy,private$.runner)
                 ladd(private$.smartObjs)<-aSmartObj

                 ## model-implied correlations among the variables (filled from
                 ## obj$info$Sigma); columns are added at run from the matrix.
                 aSmartObj                <- SmartTable$new(self$results$implied_cors,private$.runner)
                 aSmartObj$expandOnRun    <- TRUE
                 aSmartObj$expandFrom     <- 2
                 ladd(private$.smartObjs) <- aSmartObj

                           
                 aSmartObj<-SmartTable$new(self$results$customtable,private$.runner)
                 aSmartObj$hideOn<-list("z"=NA)
                 ladd(private$.smartObjs)<-aSmartObj
                 
                 ### init all ####
                 for (tab in private$.smartObjs) {
                     tab$initTable()
                 }

      
        }, ## end of init
        .run = function() {
                now <- Sys.time()
                jinfo(paste("MODULE:  PAMLcorr #### phase run  ####"))
                 private$.runner$run()
                 if (identical(private$.runner$mode, "medsimple") &&
                     private$.runner$ok) {
                     medArgs <- list(A = private$.runner$info$A,
                                     Sigma = private$.runner$info$Sigma,
                                     S = private$.runner$info[["S"]],   # exact: avoid $S partial-matching "Sigma"
                                     sig.level = self$options$sig.level,
                                     alternative = self$options$alternative,
                                     test = self$options$test,
                                     R = self$options$mcR,
                                     parallel = self$options$parallel)
                     if (identical(private$.runner$aim, "n"))
                         medArgs$power <- self$options$power
                     if (identical(private$.runner$aim, "power"))
                         medArgs$n <- self$options$n
                     if (identical(private$.runner$aim, "es")) {
                         medArgs$n <- self$options$n
                         medArgs$power <- self$options$power
                         ## which coefficient to resize (a vs b), resolved in .checkdata.medsimple
                         medArgs$vary_edge <- private$.runner$info[["vary_edge"]]
                     }
                     if (isTRUE(self$options$set_seed)) medArgs$seed <- self$options$seed
                     private$.runner$data <- as.data.frame(do.call(pamlj.mediation, medArgs))
                 }
                 private$.plotter$preparePlots()
                 for (tab in private$.smartObjs) {
                     tab$runTable()
                 }
                 if (identical(private$.runner$mode, "medsimple") && private$.runner$ok)
                     self$results$powertab$setRow(rowNo=1, private$.runner$data)
                 private$.runner$endrun()                 
      jinfo("MODULE:  #### phase end ####")

      jinfo("TIME:",Sys.time()-private$.time," secs. Runtime: ",Sys.time()-now)
                 

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

             ## free (syntax) models are laid out by semPlot from a lavaan model;
             ## simple / complex models use the hand-built qgraph layout below.
             if (!is.null(image$state$model)) {
                 state <- image$state
                 semPlot::semPaths(state$model, whatLabels = "est",
                                   layout   = state$layout,
                                   sizeLat  = state$sizeLat,  sizeLat2 = state$sizeLat2,
                                   sizeMan  = state$sizeMan,  sizeMan2 = state$sizeMan2,
                                   edge.label.cex = state$edge.label.cex,
                                   residuals = FALSE)
                 return(TRUE)
             }

             if (is.null(image$state$coord)) return()

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
