## these are low level power functions, modified from various sources to fit the need of the module
## they take one set of parameters and return the power parameters (input+estimates)
## they should return a structure of class "paml_power" or ""power.htest" with one set of parameters
## Basically, they are used for one estimate, usually by the powervector() functions that work for multiple estimates (runs))
## they must return a `method` field

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
  
    method<-"pamlj"
    
    ncp <-function(f2,u,v) {
      switch (ncp_type,
        gpower  = {return(f2* (df_model + v+ 1))},
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
        res <- uniroot(function(f2) log(eval(p.body)) - log(power), c(1e-10, 1e+10))
        if (abs(res$f.root) > 0.01)  {
          ### this means that the required es is too small for uniroot (less than 1e-03). We go brute force (slow but correct)
          f.body <- function(f2) eval(p.body)
          int<-seq(1e-07,1e-03,length.out=1e+04)
          p<-abs(power - sapply(int,f.body))
          f2<-as.numeric(int[which.min(p)])
          print(paste("es ",f2," found by brute force. Approximation ",min(p)))
          method="brute"
        } else {
          f2<-res$root
        }
    }
    else if (is.null(sig.level)) 
        sig.level <- uniroot(function(sig.level) eval(p.body) - power, c(1e-10, 1 - 1e-10))$root
    else stop("internal error in pamlj.glm")
    n <- df_model+ ceiling(v) + 1
    return(list(u = u, v = ceiling(v), f2 = f2, sig.level = sig.level, 
        power = power, n = n, encp=ncp(f2,u,v), method=method))

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


### mediation ####

pamlj.mediation <- function(n=NULL,a=NULL,b=NULL,cprime=0,r2a=0,r2y=0,power=NULL,sig.level=.05, alternative="two.sided",test="sobel",...) {
  

  aim<-c("n","power","es")[sapply(list(n,power,a),is.null)]
  if (length(aim) != 1) stop("Only one parameter must be null in pamlj.mediation")
  
  ### this seems strange, but with this method we can have as many sequential mediation effect as we want
  # first we check if there are other coefficients other than a and b  
  obetas<-NULL
  args<-list(...)
  others<-grep("^d[1-9]",names(args))
  if (length(others)>0) {
         obetas<-unlist(args[others])
  }
   ##  checks some values
  
  if (aim != "es") {
      if (r2a==0)   r2a<-a^2
      if (r2y==0)   r2y<-b^2+cprime^2+2*a*b*cprime
  } else {
    a <- 0
    r2a <- 0
    r2y <- 0
  }

  betas<-c(a,b,obetas)
  or2s<-NULL
  others<-grep("^r2d[1-9]",names(args))
  if (length(others)>0)
         or2s<-unlist(args[others])

  r2s  <-c(r2a,r2y,or2s)  
  
  ### helping functions
  .sefun <- function(n, r2vector) {
         r2s_x<-c(0,r2vector)
         sapply(1:length(r2vector), function(i) sqrt((1 / n) * (1 - r2vector[i]) /  (1 - r2s_x[i])))
  }

     switch(test, 
     
     sobel= {
              se.formula<-function(.betas,.se) {
                               cc<-combn(.betas^2, length(.betas)-1, simplify = FALSE)
                               sqrt(sum(sapply(cc,prod) * .se^2))
                          }
            p.body <- p.sobel
            p.es   <- p.es.sobel

            },
     joint= {
       
            p.body <- p.joint
            p.es   <- p.es.joint

            }
   )

  
  
    .power.fun <- function(ncp) {
      switch (alternative,
          two.sided = {power <- 1 - pnorm(qnorm(sig.level / 2, mean = 0, sd = 1, lower.tail = FALSE), sd = 1, mean = abs(ncp)) +
                                  pnorm(-qnorm(sig.level / 2, mean = 0, sd = 1, lower.tail = FALSE), sd = 1, mean = abs(ncp))
              },
         one.sided = {
                power <- 1 - pnorm(qnorm(sig.level, mean = 0, sd = 1, lower.tail = FALSE), sd = 1, mean = abs(ncp))
              }
        )
    power
    }

     # comments and warnings variables
     attribs<-list()
     method<-"pamlj"

     switch(aim, 
            power={
                   power<-eval(p.body)
                  },
            n    ={
                   n<-try(uniroot(function(n) eval(p.body) - power, interval = c(10, 1e10))$root,silent=F)
                   # if it fails, n should be too small or to large. we test for too small
                   if ("try-error" %in% class(n)) {
                      n<-10
                      pw<-eval(p.body)
                      ## if power with 10 is larger than power, we set the minumum n=10
                      if (pw > power) {
                        method="nmin"
                        n<-10
                      } else {
                        ## otherwise, we test for too large
                      n<-1e+07
                      pw<-eval(p.body)
                      # is with n=1e+07 we do not reach the required power, we yield and say that n>1e+07
                      if (pw < power) {
                        method="nmax"
                        n<-1e+07
                      }
                      }
                   }
                  },
            es   ={
                   if (aim=="es" && length(betas)>2) stop("es can be estiamted only for simple mediation")
                   ## first we test what is the max power we can reach given b
                   x<-seq(0,1,by=.001)
                   pow<-(sapply(x,function(a) eval(p.es)))
                   .max<-max(pow)
                   ## if we can go above power, we solve for a
                   if (.max > power) {
                          a<-uniroot(function(a) eval(p.es) - power, interval = c(.00001,x[which.max(pow)] ))$root
                   }
                   else {
                          ## otherwise, we yield the effect size (a) that gives the maximum power
                          a<-x[which.max(pow)]
                          method <-"powmax"
                          power  <- .max
                          attribs$power<-.max
                   }
                   betas[1]<-a

                  }
      )

      results<-(list(n = round(n,digits=0), a = a, b=b , es= prod(betas), cprime=cprime,r2a=r2a,r2y=r2y, sig.level = sig.level,  power = power, method=method))
      attributes(results)<-c(attributes(results),attribs)
      return(results)
}

pamlj.mediation.mc <- function(n=NULL,a=NULL,b=NULL,cprime=0,r2a=0,r2y=0,
                               power=NULL,sig.level=.05, alternative="two.sided",
                               test="mc",R=1000,L=1000,parallel=FALSE,...) {

    if (Sys.info()['sysname'] == "Windows") 
                     plan<-future::multisession
    else                 
                     plan<-future::multicore
  
  future::plan(plan)

  aim<-c("n","power","es")[sapply(list(n,power,a),is.null)]
  if (length(aim) != 1) stop("Only one parameter must be null in pamlj.mediation")
  
  ### this seems strange, but with this method we can have as many sequential mediation effect as we want
  # first we check if there are other coefficients other than a and b  
  obetas<-NULL
  args<-list(...)
  others<-grep("^d[1-9]",names(args))
  if (length(others)>0)
         obetas<-args[others]

   ##  checks some values
  
  if (aim != "es") {
      if (r2a==0)   r2a<-a^2
      if (r2y==0)   r2y<-b^2+cprime^2+2*a*b*cprime
  } else {
    a <- 0
    r2a <- 0
    r2y <- 0
  }

  betas<-c(a,b,obetas)
  
  or2s<-NULL
  others<-grep("^r2d[1-9]",names(args))
  if (length(others)>0)
         or2s<-args[others]

  r2s  <-c(r2a,r2y,or2s)  

   attribs<-list()
   method<-"pamlj"
  
  ### helping functions
        .sefun <- function(n, r2vector) {
                        r2s_x<-c(0,r2vector)
                        sapply(1:length(r2vector), function(i) sqrt((1 / n) * (1 - r2vector[i]) /  (1 - r2s_x[i])))
                  }

         p.body <- quote({
           
                   se.betas   <- .sefun(n,r2s)
                   if (parallel) {
                   pw<-mean(unlist(foreach::foreach(i = 1:R, .options.future = list(seed = TRUE)) %dofuture%  {
                            # for each beta we draw a random value from its distribution
                            pars <- sapply(seq_along(betas),function(j) rnorm(1, betas[j], se.betas[j]))
                            ### than we draw a normal distribution for each parameter
                            dist <- lapply(seq_along(betas),function(j) rnorm(L, pars[j], se.betas[j]))
                            dist <- as.data.frame(do.call(cbind,dist))
                            ## and we test the 2.5th quantile of the product of the distributions
                            quantile(apply(dist,1,prod), probs=sig.level/2, na.rm = TRUE) > 0
                            }))
                   } else {
                                      
                   pw<-mean(unlist(sapply(1:R, function(i) {
                            # for each beta we draw a random value from its distribution
                            pars <- sapply(seq_along(betas),function(j) rnorm(1, betas[j], se.betas[j]))
                            ### than we draw a normal distribution for each parameter
                            dist <- lapply(seq_along(betas),function(j) rnorm(L, pars[j], se.betas[j]))
                            dist <- as.data.frame(do.call(cbind,dist))
                            ## and we test the 2.5th quantile of the product of the distributions
                            quantile(apply(dist,1,prod), probs=sig.level/2, na.rm = TRUE) > 0
                            })), na.rm=T)
                   }


                   pw
             })
          
          p.es <- quote({
                   se.a<-.sefun(n=n,a^2)
                   r2y<-b^2+cprime^2+2*a*b*cprime
                   .r2s<-c(r2y,a^2)
                   se.b<-.sefun(n=n,.r2s)
                   pw<-mean(unlist(sapply(1:R, function(i) {
                            a_par <- rnorm(1, a, se.a)
                            b_par <- rnorm(1, b, se.b)
                            quantile(rnorm(L, a_par, se.a) * rnorm(L, b_par, se.b), probs = sig.level/2, na.rm = TRUE) > 0
                            })), na.rm=T)
                   pw
             })
       



     switch(aim, 
            power={
                   power<-eval(p.body)
                  },
            n    ={
                  ### first we obtain a reasonable estimation of n
                   check<-pamlj.mediation(a=a,b=b,cprime=cprime,r2a=r2a,r2y=r2y,power=power,sig.level=sig.level, alternative=alternative,test="joint")
                   if (check$method %in% c("nmax","nmin")) return(check)
                   n_par<-check$n
                   if (n_par > 1e+06) {
                     check$method<-"nmax"
                     return(check)
                   }
                   ## now we set the search limits to reasonable values, so uniroot gets faster
                   ll<-n_par*.80
                   ul<-n_par*1.20
                   n<-try(uniroot(function(n) eval(p.body) - power, interval = c(ll, ul))$root,silent=F)
                  },
            es   ={
                   ### first we obtain a reasonable estimation of es
                   check<-pamlj.mediation(n=n,a=NULL,b=b,cprime=cprime,r2a=r2a,r2y=r2y,power=power,sig.level=sig.level, alternative=alternative,test="joint")
                   ## if powmax method is returned, we cannot do better so we stop
                   if (check$method %in% c("powmax")) return(check)
                   # now we try 
                   ll <- check$power*.95
                   ul <- check$power*1.05
                   a<-uniroot(function(a) eval(p.es) - power, interval = c(ll,ul))$root
                   r2a<-a^2
                   r2y<-b^2+cprime^2+2*a*b*cprime
                  }
      )
      results<-(list(n = round(n,digits=0), a = a, b=b , es= a*b, cprime=cprime, r2a=r2a,r2y=r2y, sig.level = sig.level,  power = power, method=method))
      attributes(results)<-c(attributes(results),attribs)
      return(results)
}


##### mediation power functions

          p.sobel <- quote({
                se.betas   <- .sefun(n,r2s)
                se         <-  se.formula(betas, se.betas)
                ncp        <-  prod(betas) / se
                pw         <- .power.fun(ncp) 
                pw
                })
          
            p.es.sobel <- quote({
                
                r2s[1]     <-  a^2
                r2s[2]     <-  b^2+cprime^2+2*a*b*cprime
                se.betas   <- .sefun(n,r2s)
                betas[1]   <-  a
                se         <-  se.formula(betas, se.betas)
                ncp        <-  prod(betas) / se
                pw         <- .power.fun(ncp) 
                pw
                })
            
            p.joint <- quote({
                se.betas   <- .sefun(n,r2s)
                ncp        <-  betas / se.betas
                pw         <-  sapply(ncp, .power.fun) 
                prod(pw)
                })
            
            p.es.joint <- quote({
                r2s[1]     <-  a^2
                r2s[2]     <-  b^2+cprime^2+2*a*b*cprime
                se.betas   <- .sefun(n,r2s)
                betas[1]   <-  a
                ncp        <-  betas / se.betas
                pw         <-  sapply(ncp, .power.fun) 
                prod(pw)

                })

            
            
