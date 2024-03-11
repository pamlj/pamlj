### computes the actual power parameters

powerfunction <- function(x, ...) UseMethod(".powerfunction")


.powerfunction.correlation <- function(obj) {
  

    if (obj$tails == "two") 
           alt="two.sided"
    else {
      if (obj$data[["es"]]>0)
          alt="greater"
      else 
          alt="less"
    }
    res<-pwr::pwr.r.test(n=obj$data[["n"]],r=obj$data[["es"]],power=obj$data[["power"]],sig.level=obj$data[["alpha"]],alternative=alt)
    obj$data[["n"]]<-round(res$n)
    obj$data[["es"]]<-res$r
    obj$data[["alpha"]]<-res$sig.level
    obj$data[["power"]]<-res$power

    warning(res$method)
    return(obj$data)
}





### computes the  power effect sizes

powerbyes <- function(x, ...) UseMethod(".powerbyes")


.powerbyes.correlation <- function(obj) {

            probs = c(.5, .8, .95)
            probs_es = sapply(probs, function(p){
              pwr::pwr.r.test(obj$data$n,
                                 sig.level = obj$data$alpha, power = p,
                                 alternative = obj$tails)$r
           })
            mark(probs_es)
            esList <-list(list(es=paste('0 <', greek_vector["rho"], greek_vector["leq"],round(probs_es[1],digits=3))),
                          list(es=paste('1 <', greek_vector["rho"], greek_vector["leq"],round(probs_es[2],digits=3))),
                          list(es=paste('2 <', greek_vector["rho"], greek_vector["leq"],round(probs_es[3],digits=3))),
                          list(es=paste(greek_vector["rho"], greek_vector["geq"],round(probs_es[3],digits=3)))
            )

            mark(esList)
            return(esList)
            
}