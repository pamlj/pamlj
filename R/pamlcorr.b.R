
# This file is a generated template, your changes will not be overwritten

pamlcorrClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "pamlcorrClass",
    inherit = pamlcorrBase,
    private = list(
        .time = NULL,
        .ready= FALSE,
        .smartObjs=list(),
        .init = function() {

                jinfo(paste("MODULE:  PAMLcorr #### phase init  ####"))
                private$.time<-Sys.time()

                private$.ready<-readiness(self$options)
                if (!private$.ready$ready) {
                         if(private$.ready$report)
                           warning("do something")
                   return()
                }

     ### set up the R6 workhorse class
                runner_machine          <-  Runner$new(self)

      
      ### info table ###
                 aSmartObj<-SmartTable$new(self$results$powertab,runner_machine)
                 ladd(private$.smartObjs)<-aSmartObj

                 aSmartObj<-SmartTable$new(self$results$powerbyes,runner_machine)
                 ladd(private$.smartObjs)<-aSmartObj
          
                 ### init all ####
                 for (tab in private$.smartObjs) {
                     tab$initTable()
                 }
                 
        }, ## end of init
        .run = function() {
          
                jinfo(paste("MODULE:  PAMLcorr #### phase run  ####"))

                 for (tab in private$.smartObjs) {
                     tab$runTable()
                 }

        })
)
