# This file is a generated template, your changes will not be overwritten

pamlsemClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "pamlsemClass",
        inherit = pamlsemBase,
        private = list(
            .time = NULL,
            .ready = FALSE,
            .smartObjs = list(),
            .plotter = NULL,
            .runner = NULL,
            .init = function() {
                jinfo(paste("MODULE:  PAMLsem #### phase init  ####"))
                private$.time <- Sys.time()
                class(private$.results) <- c("pamlj", class(private$.results)) ## this is useful in R interface


                ### set up the R6 workhorse class
                private$.runner <- Runner$new(self)

                ### handle plotter #####
                private$.plotter <- Plotter$new(self, private$.runner)
                private$.plotter$initPlots()
                ### info table ###
                aSmartObj <- SmartTable$new(self$results$powertab, private$.runner)
                ladd(private$.smartObjs) <- aSmartObj

                aSmartObj <- SmartTable$new(self$results$powerbyn, private$.runner)
                ladd(private$.smartObjs) <- aSmartObj

                aSmartObj <- SmartTable$new(self$results$implied$covs, private$.runner)
                aSmartObj$expandOnInit <- TRUE
                aSmartObj$expandFrom <- 3
                ladd(private$.smartObjs) <- aSmartObj

                aSmartObj <- SmartTable$new(self$results$implied$lvcovs, private$.runner)
                aSmartObj$expandOnInit <- TRUE
                aSmartObj$expandFrom <- 3
                ladd(private$.smartObjs) <- aSmartObj

                aSmartObj <- SmartTable$new(self$results$implied$betas, private$.runner)
                aSmartObj$expandOnRun <- TRUE
                aSmartObj$expandFrom <- 3
                ladd(private$.smartObjs) <- aSmartObj

                aSmartObj <- SmartTable$new(self$results$customtable, private$.runner)
                aSmartObj$hideOn <- list("z" = NA)
                ladd(private$.smartObjs) <- aSmartObj

                ### init all ####
                for (tab in private$.smartObjs) {
                    tab$initTable()
                }
            }, ## end of init
            .run = function() {
                now <- Sys.time()
                jinfo(paste("MODULE:  PAMLsem #### phase run  ####"))
                private$.runner$run()
                private$.plotter$preparePlots()
                for (tab in private$.smartObjs) {
                    tab$runTable()
                }
                private$.runner$endrun()

                jinfo("MODULE:  #### phase end ####")

                jinfo("TIME:", Sys.time() - private$.time, " secs. Runtime: ", Sys.time() - now)
            },
            .plot_contour = function(image, ggtheme, theme, ...) {
                private$.plotter$plot_contour(image, ggtheme, theme)

                TRUE
            },
            .plot_ncurve = function(image, ggtheme, theme, ...) {
                private$.plotter$plot_curve(image, ggtheme, theme)
                return(TRUE)
            },
            .plot_escurve = function(image, ggtheme, theme, ...) {
                private$.plotter$plot_curve(image, ggtheme, theme)
                return(TRUE)
            },
            .plot_custom = function(image, ggtheme, theme, ...) {
                private$.plotter$plot_custom(image, ggtheme, theme)
            },
            .plot_diagram = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return()
                }
                state <- image$state
                semPlot::semPaths(state$model,
                    whatLabels = "est",
                    sizeLat = state$sizeLat,
                    sizeLat2 = state$sizeLat2,
                    sizeMan = state$sizeMan,
                    sizeMan2 = state$sizeMan2,
                    edge.label.cex = state$edge.label.cex,
                    residuals = FALSE
                )
                return(TRUE)
            }
        ) # end of private
    )
} # end of class
