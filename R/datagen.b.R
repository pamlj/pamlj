
datagenClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "datagenClass",
    inherit = datagenBase,
    private = list(
        .time=NULL,
        .ready=NULL,
        .dispatcher=NULL,
        .data_machine=NULL,
        .runner=NULL,
        .smartObjs=NULL,
        .init=function() {
          

          ginfo(paste("MODULE: DataGen  #### phase init  ####"))
          
          private$.time <- Sys.time()

          if (!self$options$output)
             self$results$start$setContent(INTRO)
          
          ### set up the R6 workhorse class
          private$.dispatcher               <- Dispatch$new(self$results)
          private$.runner                   <- Runner$new(self$options, private$.dispatcher,self$data)

          ## prepare stuff
          private$.dispatcher$clean(self$results$start)
          private$.dispatcher$clean(self$results$help)
          private$.runner$prepare()
          
          if (!private$.runner$ready)
             return()

          
          
          ### info table ###
          aSmartObj<-SmartTable$new(self$results$info,private$.runner)
          ladd(private$.smartObjs)<-aSmartObj
          
          
          ### init all ####
          for (tab in private$.smartObjs) {
            tab$initTable()
          }
          
                    
          now <- Sys.time()
          ginfo("INIT TIME:", now - private$.time, " secs")
          
        },
        .run = function() {

          ginfo("MODULE:  #### phase run ####")
          
          runnow <- Sys.time()

          if (!private$.runner$ready)
            return()
          
          private$.runner$run()

          ### run tables ###
          for (smarttab in private$.smartObjs) {
            smarttab$runTable()
          }
          
          private$.runner$savedata(self$results)
          
          
          ginfo("MODULE:  #### phase end ####")
          
          ginfo("RUN TIME:", Sys.time() - runnow, " secs")
          
          ginfo("TIME:", Sys.time() - private$.time, " secs")
          
          

        })
)
