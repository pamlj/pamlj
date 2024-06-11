## these are low level power functions, modified from various sources to fit the need of the module
## they take one set of parameters and return the power parameters (input+estimates)
## they should return a structure of class "paml_power" or ""power.htest" with one set of parameters
## Basically, they are used for one estimate, usually by the powervector() functions that work for multiple estimates (runs))


pamlj.glm <- function(u=NULL,v=NULL,f2=NULL,power=NULL,sig.level=NULL,df_model=NULL,ncp_type="gpower",  alternative="two.sided") {
  
  
    if (!is.null(v) && !is.null(power) && !is.null(f2) && !is.null(sig.level)) 
        stop("too many parameters v=",v," power=",power," es=",f2," sig.level=",sig.level)
  
    if (alternative=="one.sided" ) {
         if ( is.something(sig.level) )
               sig.level <- sig.level * 2
         else 
             stop("The required power parameter cannot be computed for one-tailed tests")
    }
    if (is.null(df_model))
         stop("df_model must be defined")
  
    ncp <-function(f2,u,v) {
      switch (ncp_type,
        gpower  = {return(f2* (df_model + v+ 1))},
        liberal = {return(f2*(u+v+1))},
        strict  = {return(f2*v)}
      )
    }

p.body <- quote({
        lambda <- ncp(f2 , u, v)
        pf(qf(sig.level, u, v, lower.tail = FALSE), u, v, lambda, lower.tail = FALSE)
    })
    if (is.null(power)) 
        power <- eval(p.body)
    else if (is.null(u)) 
        u <- uniroot(function(u) eval(p.body) - power, c(1 + 
            1e-10, 100))$root
    else if (is.null(v)) 
        v <- uniroot(function(v) eval(p.body) - power, c(1 + 
            1e-10, 1e+09))$root
    else if (is.null(f2)) 
        f2 <- uniroot(function(f2) eval(p.body) - power, c(1e-07, 
            1e+07))$root
    else if (is.null(sig.level)) 
        sig.level <- uniroot(function(sig.level) eval(p.body) - 
            power, c(1e-10, 1 - 1e-10))$root
    else stop("internal error in pamlj.glm")
    n <- df_model+ ceiling(v) + 1
    c(u = u, v = ceiling(v), f2 = f2, sig.level = sig.level, 
        power = power, n = n, encp=ncp(f2,u,v))

}
### These two functions are from jpower https://github.com/richarddmorey/jpower/blob/master/jpower/R/utils.R

pamlj.ttestind<-function(n= NULL, n_ratio=NULL, n1 = NULL, n2 = NULL, d = NULL, sig.level = NULL, power = NULL, alternative = "two.sided") {
  
    if(is.null(n)) 
       ret <- pamlj.t2n.ratio(n_ratio = n_ratio, d=d, sig.level=sig.level, power=power, alternative=alternative)
    else
       ret <- pamlj.t2n.test(n1 = n1, n2 = n2, d=d, sig.level=sig.level, power=power, alternative=alternative)
      
    return(ret)  
}

pamlj.t2n.test = function(n1 = NULL, n2 = NULL, d = NULL, sig.level = NULL, power = NULL, alternative = c("two.sided", "less", "greater")){

    if(!is.null(power))
    if(power>=1) stop("Power cannot be 1.")
  if(is.null(d)){
    if(power<sig.level) stop("power < alpha")
    x = try(pwr::pwr.t2n.test(n1 = n1, n2 = n2, d = d, sig.level = sig.level, power = power, alternative = alternative), silent = TRUE)
    if(inherits(x, "try-error")){
      effN = n1 * n2 / (n1 + n2)
      df = n1 + n2 - 2
      if( length(alternative) >1 ) alternative == alternative[1]
      if(alternative == "two.sided"){
        crit = qt(1 - sig.level/2, df)
        es = uniroot(function(x){
          d = exp(log(x) - log1p(-x))
          ncp = d * sqrt(effN)
          pow = pt(-crit, df, ncp) + 1 - pt(crit, df, ncp)
          log(pow) - log(power)
        }, interval = c(0,1))$root
        d = exp(log(es) - log1p(-es))
      }else if(alternative %in% c("greater","less")){
        crit = qt(1 - sig.level, df)
        es = uniroot(function(x){
          d = exp(log(x) - log1p(-x))
          ncp = d * sqrt(effN)
          pow = 1 - pt(crit, df, ncp)
          log(pow) - log(power)
        }, interval = c(0,1))$root
        d = exp(log(es) - log1p(-es))
        d = ifelse(alternative == "less", -d, d)
      }else{
        stop("Invalid alternative")
      }
      METHOD <- c("t test power calculation")
      ret = structure(list(n1 = n1, n2 = n2, d = d, sig.level = sig.level,
                           power = power, alternative = alternative, method = METHOD),
                      class = "power.htest")
      return(ret)
    }else{
      return(x)
    }
  }else{
    return(pwr::pwr.t2n.test(n1 = n1, n2 = n2, d = d, sig.level = sig.level, power = power, alternative = alternative))
  }
}


pamlj.t2n.ratio = function(n_ratio = 1, d, sig.level, power, alternative){
  
  if(power>=1) return(Inf)
  fn = Vectorize(function(n1){
    effN = n1 * n_ratio /  (1 + n_ratio)
    df = n1 * (1 + n_ratio) - 2
    ncp = sqrt(effN) * d
    if(alternative == "two.sided"){
      critt = qt(sig.level/2, df)
      pow = pt(critt, df, ncp) + 1 - pt(-critt, df, ncp)
    }else if(alternative == "less"){
      critt = qt(sig.level, df)
      pow = pt(critt, df, ncp)
    }else if(alternative == "greater"){
      critt = qt(1 - sig.level, df)
      pow = 1 - pt(critt, df, ncp)
    }else{
      stop("Invalid alternative.")
    }
    return(log(pow) - log(power))
  }, "n1")
  rt = uniroot(fn, c(ceiling( 3 / (1 + n_ratio) ), 1e+09))$root
  n1 <- ceiling(rt)
  n2 <- ceiling(n1*n_ratio)
  
  ret = structure(list(n1 = n1, n2 = n2, d = d, sig.level = sig.level,
                           power = power, alternative = alternative),
                      class = "power.htest")
  ret

}

### independent samples proportions ###

pamlj.propind<-function(n= NULL, n_ratio=NULL, n1 = NULL, n2 = NULL, h = NULL, sig.level = NULL, power = NULL, alternative = "two.sided") {
  
    if(is.null(n)) 
       ret <- pamlj.p2n.ratio(n_ratio = n_ratio, h=h, sig.level=sig.level, power=power, alternative=alternative)
    else
       ret <- pwr::pwr.2p2n.test(n1 = n1, n2 = n2, h=h, sig.level=sig.level, power=power, alternative=alternative)
      
    return(ret)  
}


pamlj.p2n.ratio<-function(n_ratio = n_ratio, h=h, sig.level=sig.level, power=power, alternative=alternative) {
  
  fn<-function(n1) { 
     n2<-n1*n_ratio
     pp<-pwr::pwr.2p2n.test(n2 = n2, n1 = n1, h = h, sig.level = sig.level, alternative = alternative)
     return(log(pp$power) - log(power))
   }
   n1 = ceiling(uniroot(fn, c( 2, 1e+09))$root)
   n2 = n1*n_ratio
   ret = structure(list(n1 = n1, n2 = n2, h = h, sig.level = sig.level,
                           power = power, alternative = alternative),
                           class = "power.htest")
  ret

}
  
 
### This is adjusted from MESS  power_mcnemar_test code. 

pamlj.prop.paired <- function (n = NULL, p1 = NULL, psi = NULL, sig.level = 0.05, 
    power = NULL, alternative = c("two.sided", "greater")) {
      
    if (sum(sapply(list(n, p1, psi, power, sig.level), is.null)) !=       1) {
        stop("exactly one of 'n', 'p1', 'psi', 'power', and 'sig.level' must be NULL")
    }
    if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > sig.level | sig.level > 1)) 
        stop("'sig.level' must be numeric in [0, 1]")
      
    if (!is.null(p1)) {
        if (any(p1 <= 0) || any(p1 >= 0.5)) {
            stop("p1 is the smallest discordant probability and must be 0<p1<0.5")
        }
    }
    if (!is.null(psi)) {
        if (any(psi <= 1)) {
            stop("psi must be 1 or greater since it is the ratio of the larger discordant probability to the smaller discordant probability")
        }
        if (any((psi + 1) * p1 > 1)) {
            stop("psi cannot be so big that the sum of the discordant probabilities exceed 1: ie., (1+p1)*psi>1")
        }
    }
    alternative <- match.arg(alternative)

    tside <- switch(alternative, greater = 1, two.sided = 2)
    f <- function(n, p1, psi, sig.level, power) {
        bc <- ceiling(p1 * n * (1 + psi))
        pbinom(qbinom(sig.level/tside, size = bc, prob = 0.5) - 
            1, size = bc, prob = 1/(1 + psi)) + 1 - pbinom(qbinom(1 - 
            sig.level/tside, size = bc, prob = 0.5), size = bc, 
            prob = 1/(1 + psi))
    }
        p.body <- quote(pnorm((sqrt(n * p1) * (psi - 1) - qnorm(sig.level/tside, 
            lower.tail = FALSE) * sqrt(psi + 1))/sqrt((psi + 
            1) - p1 * (psi - 1)^2)))
        
    if (is.null(power)) {
        power <- eval(p.body)
    }
    else if (is.null(n)) {
        n <- uniroot(function(n) eval(p.body) - power, c(ceiling(log(sig.level)/log(0.5)), 
            1e+07))$root
    }
    else if (is.null(p1)) 
        p1 <- uniroot(function(p1) eval(p.body) - power, 
            c(1e-10, 1/(1 + psi) - 1e-10))$root
    else if (is.null(psi)) 
        psi <- uniroot(function(psi) eval(p.body) - power, c(1 + 
            1e-10, 1/p1 - 1 - 1e-10))$root
    else if (is.null(sig.level)) 
        sig.level <- uniroot(function(sig.level) eval(p.body) - 
            power, c(1e-10, 1 - 1e-10))$root
    else stop("internal error", domain = NA)
    structure(list(n = n, p1 = p1, psi = psi, sig.level = sig.level, 
        power = power, alternative = alternative), class = "power.htest")
}
