
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

  whichnull<-setdiff(c("n","es","sig.level","power"), names(data))  
  if (length(whichnull)>1 || length(whichnull)==0)
         stop("PAMLj: only one parameters should be NULL")
  whichnull
}

nicify_param<- function(what,short=FALSE) {

  if (short) {
  name=switch (what,
       n  = "N",
       es = "ES",
       power= "Power",
       alpha = "Alpha"
      )
    return(name)
  }  
  switch (what,
    n  = "Sample size (N)",
    es = "Effect size",
    power= "Power",
    alpha = "Required critical alpha"
  )
}

niceround<-function(x) {
  x<-round(x,3)
  a<-round(x,1)
  w<- abs(x-a) < .001
  x[w]<-round(x[w],1)
  x
}




