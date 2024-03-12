TRANS_WARNS=list()

paml_palette<- function(...) viridis::viridis(...,alpha=.7)




fields_tothem <- function(obj, ...) UseMethod(".fields_tothem")


.fields_tothem.correlation <- function(obj,names) {
  
   names[names=="alpha"]<-"sig.level"
   names[names=="es"]<-"r"
   return(names)

}

fields_tome <- function(obj, ...) UseMethod(".fields_tome")


.fields_tome.correlation <- function(obj,names) {
  
   names[names=="sig.level"]<-"alpha"
   names[names=="r"]<-"es"
   return(names)

}



checkdata <- function(obj, ...) UseMethod(".checkdata")

.checkdata.correlation <- function(obj) {
  
  if (is.something(obj$data$es)) {
     if (abs(obj$data$es)<.001)
         stop("Correlation absolute value cannot be less than .001")
     if (abs(obj$data$es)>.99)
         stop("Correlation absolute value cannot be more than .99")

  }
  
}
