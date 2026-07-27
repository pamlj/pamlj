## these are low level power functions, modified from various sources to fit the need of the module
## they take one set of parameters and return the power parameters (input+estimates)
## they should return a structure of class "paml_power" or ""power.htest" with one set of parameters
## Basically, they are used for one estimate, usually by the powervector() functions that work for multiple estimates (runs))
## they must return a `method` field

pamlj.glm <- function(u=NULL,v=NULL,f2=NULL,power=NULL,sig.level=NULL,df_model=NULL,ncp_type="model",  alternative="two.sided") {
  
  
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
  
    method<-"pamlj"
    
    ncp <-function(f2,u,v) {
      switch (ncp_type,
        model  = {return(f2* (df_model + v+ 1))},
        liberal = {return(f2*(u+v+1))},
        strict  = {return(f2*v)}
      )
    }
    p.body <- quote({
        lambda <- ncp(f2 , u, v)
        pow<-pf(qf(sig.level, u, v, lower.tail = FALSE), u, v, lambda, lower.tail = FALSE)
        pow 
    })
    if (is.null(power)) 
        power <- eval(p.body)
    else if (is.null(u)) 
        u <- uniroot(function(u) eval(p.body) - power , c(1 + 
            1e-10, 100))$root
    else if (is.null(v)) 
        v <- uniroot(function(v) eval(p.body) - power, c(1 + 
            1e-10, 1e+09))$root
    else if (is.null(f2)) {
        log_root <- try(
            uniroot(
                function(log_f2) {
                    f2 <- exp(log_f2)
                    log(eval(p.body)) - log(power)
                },
                c(log(1e-16), log(1e+10))
            ),
            silent = TRUE
        )

        if (!inherits(log_root, "try-error") && is.finite(log_root$root) && abs(log_root$f.root) <= 0.01) {
            f2 <- exp(log_root$root)
        } else {
          ### log-space uniroot failed or was imprecise; fall back to optimize() over the full range
          f.body <- function(f2) eval(p.body)
          result <- optimize(function(f2) abs(power - f.body(f2)), interval = c(1e-16, 1e+10))
          f2 <- result$minimum
          rmsg_warn(paste("es", round(f2, 6), "found by optimize fallback"))
          method <- "brute"
        }
    }
    else if (is.null(sig.level)) 
        sig.level <- uniroot(function(sig.level) eval(p.body) - power, c(1e-10, 1 - 1e-10))$root
    else stop("internal error in pamlj.glm")
    n <- df_model+ ceiling(v) + 1
    return(list(u = u, v = ceiling(v), f2 = f2, sig.level = sig.level, 
        power = power, n = n, encp=ncp(f2,u,v), method=method))

}

### this is taken verbatim from https://github.com/mcfanda/gzlmpower

pamlj.gzlm <- function(es=NULL,prob,df=NULL, n=NULL,sig.level=.05, power=NULL) {
  
  if (is.null(prob))
    stop("Dependent variable levels proportion should be defined")
  
  if (sum(sapply(list(es, n, df, power, sig.level), is.null)) !=  1)
    stop("exactly one of es, N, df, power, and sig.level must be NULL")
  if (sum(prob)!=1)
    stop("Dependent variable levels proportion should sum to 1")
  D0<--2*sum(prob*log(prob))
  lambda<-sqrt(es*D0)
  if (length(lambda)==0) lambda<-NULL
  res<-pwr::pwr.chisq.test(lambda,df=df,power = power,sig.level = sig.level,N=n)
  res$n<-res$N
  res$N<-NULL
  if (is.null(es))
    res$w<-res$w^2/D0
  else
    res$w<-es
  
  names(res)[1]<-"es"
  # The proportional-odds (ordinal) model always carries exactly 1 df per predictor
  # regardless of the number of outcome categories, unlike the nominal (multinomial)
  # coding this check otherwise assumes -- so that specific, valid pattern is exempted
  # rather than flagged as a likely df mistake.
  if ((length(prob)-1)>df && !(df==1 && length(prob)>2))
    stop("DF are less than the number of dummies representing the dependent variable. Please be sure that the df are correct.")
  res
  
}

### These two functions are from jpower https://github.com/richarddmorey/jpower/blob/master/jpower/R/utils.R with some adjustment

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

      ret = structure(list(n1 = n1, n2 = n2, d = d, sig.level = sig.level,
                           power = power, alternative = alternative, method = "pamlj"),
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
                           power = power, alternative = alternative, method="pamlj"),
                      class = "power.htest")
  ret

}

### independent samples proportions ###


pamlj.propind<-function(n= NULL, n_ratio=NULL, n1 = NULL, n2 = NULL, h = NULL, sig.level = NULL, power = NULL, alternative = "two.sided") {
  
  
    if(is.null(n)) 
       ret <- pamlj.p2n.ratio(n_ratio = n_ratio, h=h, sig.level=sig.level, power=power, alternative=alternative)
    else
       ret <- pwr::pwr.2p2n.test(n1 = n1, n2 = n2, h=h, sig.level=sig.level, power=power, alternative=alternative)
    
    ret$method<-"pamlj"
    return(ret)  
}


pamlj.p2n.ratio<-function(n_ratio, h, sig.level=NULL, power=NULL, alternative="two.sided") {
  

  fn<-function(n1) { 
     n2<-n1*n_ratio
     pp<-pwr::pwr.2p2n.test(n2 = n2, n1 = n1, h = h, sig.level = sig.level, alternative = alternative)
     return(log(pp$power) - log(power))
  }
  
   ## min group n should be 2. If n_ratio is less than 1, the algorithm should start from a n1 that allows
   ## n2 tobe at least 2
   start<-2
   if (round(n_ratio*2) < 2)
     start<-round(2/n_ratio)
   n1 = ceiling(uniroot(fn, c( start, 1e+09))$root)
   n2 = n1*n_ratio
   ret = structure(list(n1 = n1, n2 = n2, h = h, sig.level = sig.level,
                           power = power, alternative = alternative, method="pamlj"),
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
            1e+10))$root
    }
    else if (is.null(p1)) 
        p1 <- uniroot(function(p1) eval(p.body) - power, 
            c(1e-10, 1/(1 + psi) - 1e-10))$root
    else if (is.null(psi)) {
        psi <- uniroot(function(psi) eval(p.body) - power, c(1 +1e-10, 1/p1 - 1 - 1e-10))
        psi<-psi$root
    }
    else if (is.null(sig.level)) 
        sig.level <- uniroot(function(sig.level) eval(p.body) - 
            power, c(1e-10, 1 - 1e-10))$root
    else stop("internal error", domain = NA)
    structure(list(n = round(n,digits=0), p1 = p1, psi = psi, sig.level = sig.level, 
        power = power, alternative = alternative,method="pamlj"), class = "power.htest")
}


##### sem power functions
            
            
pamlj.semmc <- function(type,
                        modelH0,
                        modelH1,
                        modelPop,
                        sigma,
                        alpha,
                        estimator,
                        n=NULL, 
                        power=NULL,
                        test="lrt",
                        parallel=FALSE,
                        seed=NULL,
                        R=250,...) {
  
    if (parallel) {
       jinfo("SEM MC goes parallel")
       if (Sys.info()['sysname'] == "Windows") 
                     plan<-future::multisession
       else                 
                     plan<-future::multicore
  
       RNGkind("L'Ecuyer-CMRG")
       future::plan(plan)
  } else  jinfo("SEM MC does not go parallel")

  
  if (is.something(seed)) set.seed(seed)

  results<-list( type=type, 
                "rmsea"=0,
                "requiredN"=ifelse(is.null(n), NA,n),
                "impliedPower"=ifelse(is.null(power), NA,power),
                "chiCrit"=NA,
                "df"=  NA,
                "alpha"=alpha,
                "modelH0"=modelH0,
                "modelH1"=modelH1,
                "modelPop"=modelPop,
                estimator=estimator)
  
  lrtfun<-function(n) {
        data<-MASS::mvrnorm(n=round(n),rep(0,ncol(sigma)),Sigma=sigma)
        mod0<-suppressWarnings(try(lavaan::sem(modelH0,data=data)))
        mod1<-suppressWarnings(try(lavaan::sem(modelH1,data=data)))
        if ("try-error" %in% class(mod0)) return(NA)
        if (!lavaan::lavInspect(mod0,"converged")) return(NA)
        if ("try-error" %in% class(mod1)) return(NA)
        if (!lavaan::lavInspect(mod1,"converged")) return(NA)
        res<-as.numeric(lavaan::lavTestLRT(mod0,mod1)$`Pr(>Chisq)`[2]<alpha)
        res
  }
  
    scorefun<-function(n) {
        data<-MASS::mvrnorm(n=round(n),rep(0,ncol(sigma)),Sigma=sigma)
        mod0<-suppressWarnings(try(lavaan::sem(modelH0,data=data)))
        if ("try-error" %in% class(mod0)) return(NA)
        if (!lavaan::lavInspect(mod0,"converged")) return(NA)
        res<-as.numeric(lavaan::lavTestScore(mod0)$test["p.value"]<alpha)
        res
  }

    switch (test,
      lrt = {repfun<-lrtfun},
      score= {repfun<-scorefun}
    )
  
  
  p.body <- quote({
            
                   if (parallel) {
                     sims<-unlist(foreach::foreach(i = 1:R, .options.future = list(seed = TRUE)) %dofuture%  repfun(n) )
                     good<-sims[!sapply(sims,is.na)]
                     res<-list(power=mean(good),conv=length(good)/R)
                   } else {
                     sims<-unlist(sapply(1:R, function(i) repfun(n) )) 
                     good<-sims[!sapply(sims,is.na)]
                     res<-list(power=mean(good),conv=length(good)/R)
                   }
                   res
             })
   ### fixed indices
   
   switch (type,
     'post-hoc' = {
              results<-eval(p.body)
              results$n<-n
             },
    'a-priori' = {
               args<-list(type="a-priori",
                          modelPop=modelPop,
                          modelH0=modelH0,
                          modelH1=modelH1,
                          estimator=estimator,
                          alpha=alpha,
                          power=power)
               res_par<-do.call(pamlj.semanalytic,args)
               n_par<-res_par$n
               ll<-n_par*.80
               ul<-n_par*2
               results$n<-n_par
               myeval<-function(x) round(x$power-power,digits=2)
               nobj<-try_hard(uniroot(function(n) myeval(eval(p.body)), interval = c(ll, ul))$root,silent=F)
               if (!isFALSE(nobj$error)) stop(nobj$error)
               results$n<-nobj$obj
             }     
   )
  
       modobj<-try_hard(lavaan::sem(modelH0,sample.cov=sigma,sample.nobs=results$n))
        if (!isFALSE(modobj$error) ||  !lavaan::lavInspect(modobj$obj,"converged")) {
          results$es<-NA
          results$test<-NA
          results$df<-NA
        } else {
          indices<-lavaan::fitmeasures(modobj$obj)
          results$es<-indices[["rmsea"]]
          results$test<-indices[["chisq"]]
          results$df <-lavaan::lavTestScore(modobj$obj)$test[["df"]]
         }
  
  return(results)
    
}
            

pamlj.semanalytic <- function(type,modelH0,modelH1,modelPop,alpha,estimator,n=NULL,power=NULL,...) {
    
     results<-semPower::semPower.powerLav(type=type,
                                    modelH0=modelH0,
                                    modelH1=modelH1,
                                    modelPop=modelPop,
                                    fittingFunction=estimator,
                                    fittingH1model=FALSE,
                                    alpha=alpha,
                                    power=power,
                                    N=n,
                                    plotShow=FALSE
                                    )
     if (type=="a-priori") {
       results<-results[c("rmsea","impliedPower","requiredN","df","chiCrit")]
       names(results)<-c("es","power","n","df","test")
     } else {
       results<-results[c("rmsea","power","requiredN","df","chiCrit")]
       names(results)<-c("es","power","n","df","test")
     }
      results
}
