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




required_param<-function(data) {
  
  whichnull<-setdiff(c("n","es","alpha","power"), names(data))  
  if (length(whichnull)>1)
         stop("FUNCTION powervecot: only one parameters should be NULL")
  if (length(whichnull)==0)
         stop("FUNCTION powervector: exactly one parameters should be NULL")
  whichnull
}