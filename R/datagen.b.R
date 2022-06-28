
datagenClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "datagenClass",
    inherit = datagenBase,
    private = list(
        .init=function() {
         mark("testme init")
        },
        .run = function() {

          mark("testme run")
          
            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
            dep<-self$options$dep
            aTable<-self$results$info
            
            if (!is.something(dep))
                 return()
            
            aTable$addRow(rowKey=1,list(info="Saved Columns",specs="Residuals",value=0))
            aTable$addRow(rowKey=2,list(info="Saved Columns",specs="Multi Residuals", value=0))
            

            if (self$options$residuals && self$results$residuals$isNotFilled()) {
                aTable$setRow(rowKey=1,list( value=1))
                
                p<-self$data[[dep]]-mean(self$data[[dep]])
                # we need the rownames in case there are missing in the datasheet
                pdf <- data.frame(residuals=p, row.names=rownames(self$data))
                self$results$residuals$setValues(pdf)
            }
            
            if (self$options$multi_residuals && self$results$multi_residuals$isNotFilled()) {

                 n<-nrow(self$data)
                 cols<-rbinom(1,3,.5)+1
                 aTable$setRow(rowKey=2,list( value=cols))
                 
                 pdf <- data.frame(matrix(rnorm(n*cols),ncol=cols,nrow =n ))
                 names(pdf)  <- paste0("MULTI_RES_",self$options$dep,1:cols)  
                 # we need the rownames in case there are missing in the datasheet
                 self$results$multi_residuals$set(1:cols,
                                       names(pdf),
                                       rep("residuals",cols),
                                       rep("continuous",cols))
                 self$results$multi_residuals$setValues(pdf)
            }
            
        })
)
