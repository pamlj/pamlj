
es.prepare<- function(objlist,...) UseMethod(".prepare")

.prepare.default<-function(objlist,runner)  stop("No prepare function for this class ",paste(class(objlist),collapse = ", "))

.prepare.es_type_d<- function(objlist,runner) {
  
   jinfo("Preparing ES for d")
  .class<-class(objlist)

   objlist<-lapply(objlist, function(x) {
                  x$value<-as.numeric(x$value)
                  x$beta<-x$value/sqrt(x$value^2+4)
                  x$original<-x$value
                  x
     })  
   
   ss<-sum(unlist(lapply(objlist, function(x) x$beta^2)),na.rm = TRUE)
   if (runner$option("r2_input")) 
        r2<-as.numeric(runner$options$r2_value)
   else {
     
        r2<-ss
        if (length(runner$options$fixed_sizes)>1)
           runner$warning<-list(topic="coefficients",message="Independent variables are assumed to be uncorrelated.                                                            If they are not, please input the expected R-squared.")
   }
   if (r2>1)
       stop("The R-squared is impossible, it must be less than 1.")
   if (ss>r2)
      stop("The R-squared is impossible, it must be more than the sum of squares of the betas.")

   ## compute the degrees of freedom

   eslist<-lapply(objlist,function(x) {
     x$df<-1
     x$eta2<-x$beta^2/(1+x$beta^2-r2)
     x$f2<-x$eta2/(1-x$eta2)
      if (x$f2>1)
        stop("The effect size for variable ", x$name," is impossible, please correct it.")

     return(x)
   })
   alldf<-sum(unlist(sapply(eslist, function(x) x$df)))
   attr(eslist,"alldf")<-alldf
   attr(eslist,"r2")<-r2
   class(eslist)<-.class
   jinfo("Preparing ES for d: DONE")
   return(eslist)
}

es.variances<- function(objlist,...) UseMethod(".variances")


.variances.es_type_d<- function(objlist,runner) {
   
   jinfo("Preparing Variances")
   .class<-class(objlist)
   .terms<-unlist(rlist::list.map(objlist,name))
    ef_list<-runner$options$model_terms
    
    ef_list<-lapply(ef_list,function(x) {
       mark(x)
       if (x %in% .terms)
           return(rlist::list.find(objlist,name==x))
       term<-list(name=x)
       found<-rlist::list.find(objlist,stringr::str_detect(name,paste0("^",x,"[1-9]")),n=Inf)
       beta<-c()
       for (f in found)
           beta<-c(beta,f$beta*2)
       k<-length(beta)
       mark(k,beta)
       dummies<-contr.treatment(k+1)-(1/(k+1))
       ss<-dummies%*%beta
       term$eta2 <- sum(ss^2)/(k+1)
       term$f2   <- term$eta2/(1-term$eta2)
       term$df   <-k
       return(term)
    })

    return(ef_list)
}




es.postpare<- function(objlist,...) UseMethod(".postpare")

.postpare.default<-function(objlist,runner)  stop("No postpare function for this class ",paste(class(objlist),collapse = ", "))

.postpare.es_type_d<- function(objlist,runner) {

  jinfo("Postparing ES for d")

  r2<-attr(runner$es_list,"r2")
  objlist<-lapply(objlist, function(x) {
    x$beta <- 2*sqrt(x$f2*(1-r2))
    x$es <- x$beta/sqrt(1-r2)
    x$t  <- sqrt(x$f2)
    x
  })
  attr(objlist,"titles")<-list(es="d")
  jinfo("Postparing ES for d: DONE")
  return(objlist)
}