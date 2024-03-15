Datamatic <- R6::R6Class(
  "Datamatic",
  cloneable=FALSE,
  class=TRUE,
  inherit = Scaffold,
  public=list(
    vars=NULL,

    initialize=function(options,dispatcher,data) {
      
      super$initialize(options,dispatcher)

    },
    
    cleandata=function(data) {
      
      return(data)
      
    }

  ), ### end of public
  private=list(

  ) #end of private
)

