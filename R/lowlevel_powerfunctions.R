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

pamlj.mediation <- function(n=NULL,a=NULL,b=NULL,cprime=0,r2a=0,r2y=0,power=NULL,sig.level=.05, alternative="two.sided",test="sobel",R=1000,precise=TRUE,parallel=FALSE,...) {
  
  R=R*10
  if (parallel && test=="joint" && isTRUE(precise)) {
    if (Sys.info()['sysname'] == "Windows")
      plan<-future::multisession
    else
      plan<-future::multicore
    RNGkind("L'Ecuyer-CMRG")
    future::plan(plan)
    jinfo("MEDIATION JOINT goes parallel")
  }
  aim<-c("n","power","es")[sapply(list(n,power,a),is.null)]
  if (length(aim) != 1) stop("Only one parameter must be null in pamlj.mediation")
  
  ### this seems strange, but with this method we can have as many sequential mediation effect as we want
  # first we check if there are other coefficients other than a and b  
  obetas<-NULL
  args<-list(...)
  joint.seed  <- if (is.something(args$joint.seed)) args$joint.seed else 12345
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
  .joint.path.rejection <- function(stat, beta, df, alpha) {
         if (alternative == "two.sided") {
            crit <- stats::qt(1 - alpha / 2, df = df)
            if (beta >= 0)
                stat > crit
            else
                stat < -crit
         } else {
            crit <- stats::qt(1 - alpha, df = df)
            if (beta >= 0)
                stat > crit
            else
                stat < -crit
         }
  }
  .with_joint_seed <- function(expr) {
         if (is.null(joint.seed))
             return(eval.parent(substitute(expr)))
         has_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
         if (has_seed)
             old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
         set.seed(joint.seed)
         out <- eval.parent(substitute(expr))
         if (has_seed)
             assign(".Random.seed", old_seed, envir = .GlobalEnv)
         else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
             rm(".Random.seed", envir = .GlobalEnv)
         out
  }
  # Recover the t statistic of one regression coefficient from the sample SSCP matrix.
  .joint_t_from_sscp <- function(W, outcome, predictors, coef_index, n) {
         xx <- W[predictors, predictors, drop = FALSE]
         xy <- W[predictors, outcome, drop = FALSE]
         yy <- W[outcome, outcome]
         inv_xx <- try(solve(xx), silent = TRUE)
         if ("try-error" %in% class(inv_xx))
             return(NA_real_)
         beta_hat <- inv_xx %*% xy
         rss <- yy - t(xy) %*% inv_xx %*% xy
         df <- n - length(predictors) - 1
         if (df <= 0 || rss <= 0)
             return(NA_real_)
         se <- sqrt((rss / df) * diag(inv_xx))
         as.numeric(beta_hat[coef_index] / se[coef_index])
  }
  .joint_power_chunk <- function(n, sigma, tests, nsim, chunk_id = 1L) {
         if (!is.null(joint.seed))
             set.seed(joint.seed + chunk_id, kind = "L'Ecuyer-CMRG")
         W <- stats::rWishart(nsim, n - 1, sigma)
         keep <- rep(TRUE, dim(W)[3])
         hits <- matrix(FALSE, nrow = dim(W)[3], ncol = length(tests))
         for (j in seq_along(tests)) {
             test <- tests[[j]]
             statsj <- vapply(seq_len(dim(W)[3]), function(i) {
                 .joint_t_from_sscp(W[, , i], test$outcome, test$predictors, test$coef_index, n)
             }, numeric(1))
             keep <- keep & is.finite(statsj)
             hits[is.finite(statsj), j] <- .joint.path.rejection(statsj[is.finite(statsj)], test$beta, n - length(test$predictors) - 1, sig.level)
         }
         if (!any(keep))
             return(c(success = NA_real_, total = 0))
         c(success = sum(apply(hits[keep, , drop = FALSE], 1, all)), total = sum(keep))
  }
  # Simulate the actual joint-significance rejection event from the implied covariance matrix.
  .joint_power_from_sigma <- function(n, sigma, tests) {
         n <- round(n)
         if (n < 4)
             return(0)
         mark(paste("joint sim: evaluating power at n =", n, "with R =", R))
         if (isTRUE(parallel)) {
             nchunks <- min(max(2, future::availableCores()), R)
             chunk_sizes <- rep(floor(R / nchunks), nchunks)
             chunk_sizes[seq_len(R %% nchunks)] <- chunk_sizes[seq_len(R %% nchunks)] + 1
             chunks <- which(chunk_sizes > 0)
             parts <- foreach::foreach(i = chunks,
                                       .options.future = list(seed = TRUE,
                                                              globals = structure(c(".joint_power_chunk", ".joint_t_from_sscp", ".joint.path.rejection",
                                                                                    "joint.seed", "sig.level", "alternative",
                                                                                    "chunk_sizes", "sigma", "tests", "n"), add = TRUE))) %dofuture% {
                 .joint_power_chunk(n = n, sigma = sigma, tests = tests, nsim = chunk_sizes[i], chunk_id = i)
             }
             parts <- do.call(rbind, parts)
             total <- sum(parts[, "total"], na.rm = TRUE)
             if (total == 0)
                 return(NA_real_)
             out <- sum(parts[, "success"], na.rm = TRUE) / total
         } else {
             W <- try(.with_joint_seed(stats::rWishart(R, n - 1, sigma)), silent = TRUE)
             if ("try-error" %in% class(W))
                 return(NA_real_)
             keep <- rep(TRUE, dim(W)[3])
             hits <- matrix(FALSE, nrow = dim(W)[3], ncol = length(tests))
             for (j in seq_along(tests)) {
                 test <- tests[[j]]
                 statsj <- vapply(seq_len(dim(W)[3]), function(i) {
                     .joint_t_from_sscp(W[, , i], test$outcome, test$predictors, test$coef_index, n)
                 }, numeric(1))
                 keep <- keep & is.finite(statsj)
                 hits[is.finite(statsj), j] <- .joint.path.rejection(statsj[is.finite(statsj)], test$beta, n - length(test$predictors) - 1, sig.level)
             }
             if (!any(keep))
                 return(NA_real_)
             out <- mean(apply(hits[keep, , drop = FALSE], 1, all))
         }
         mark(paste("joint sim: power at n =", n, "is", format(out, digits = 6)))
         out
  }
  .joint_sigma_simple <- function(a, b, cprime, r2a, r2y) {
         sigma.m <- 1 - r2a
         sigma.y <- 1 - r2y
         if (sigma.m <= 0 || sigma.y <= 0)
             return(NULL)
         var.m <- a^2 + sigma.m
         cov.xy <- cprime + a * b
         cov.my <- a * cprime + b * var.m
         var.y <- cprime^2 + (b^2 * var.m) + (2 * a * b * cprime) + sigma.y
         matrix(c(1, a, cov.xy,
                  a, var.m, cov.my,
                  cov.xy, cov.my, var.y), nrow = 3, byrow = TRUE)
  }
  .joint_simple.power <- function(n, a, b, cprime, r2a, r2y) {
         sigma <- .joint_sigma_simple(a, b, cprime, r2a, r2y)
         if (is.null(sigma))
             return(NA_real_)
         tests <- list(
             list(outcome = 2, predictors = 1, coef_index = 1, beta = a),
             list(outcome = 3, predictors = c(1, 2), coef_index = 2, beta = b)
         )
         .joint_power_from_sigma(n, sigma, tests)
  }
  # For complex models, the required path tests depend on which indirect effect row is being evaluated.
  .joint_complex.power <- function(n) {
         model_type <- as.character(args$model_type)
         effect_name <- as.character(args$effect)
         sigma <- NULL
         tests <- NULL
         if (identical(model_type, "twomeds")) {
             sigma <- diag(4)
             sigma[2,1] <- sigma[1,2] <- args$a1
             sigma[3,1] <- sigma[1,3] <- args$a2
             sigma[2,3] <- sigma[3,2] <- args$r12
             sigma[4,1] <- sigma[1,4] <- cprime + args$a1 * args$b1 + args$a2 * args$b2
             sigma[2,4] <- sigma[4,2] <- args$a1 * cprime + args$b1 + args$b2 * args$r12
             sigma[3,4] <- sigma[4,3] <- args$a2 * cprime + args$b2 + args$b1 * args$r12
             if (identical(effect_name, "a1*b1")) {
                 tests <- list(
                     list(outcome = 2, predictors = 1, coef_index = 1, beta = args$a1),
                     list(outcome = 4, predictors = c(1, 2, 3), coef_index = 2, beta = args$b1)
                 )
             } else if (identical(effect_name, "a2*b2")) {
                 tests <- list(
                     list(outcome = 3, predictors = 1, coef_index = 1, beta = args$a2),
                     list(outcome = 4, predictors = c(1, 2, 3), coef_index = 3, beta = args$b2)
                 )
             }
         } else if (identical(model_type, "threemeds")) {
             sigma <- diag(5)
             sigma[2,1] <- sigma[1,2] <- args$a1
             sigma[3,1] <- sigma[1,3] <- args$a2
             sigma[4,1] <- sigma[1,4] <- args$a3
             sigma[2,3] <- sigma[3,2] <- args$r12
             sigma[2,4] <- sigma[4,2] <- args$r13
             sigma[3,4] <- sigma[4,3] <- args$r23
             sigma[5,1] <- sigma[1,5] <- cprime + args$a1 * args$b1 + args$a2 * args$b2 + args$a3 * args$b3
             sigma[2,5] <- sigma[5,2] <- args$a1 * cprime + args$b1 + args$b2 * args$r12 + args$b3 * args$r13
             sigma[3,5] <- sigma[5,3] <- args$a2 * cprime + args$b2 + args$b1 * args$r12 + args$b3 * args$r23
             sigma[4,5] <- sigma[5,4] <- args$a3 * cprime + args$b3 + args$b2 * args$r23 + args$b1 * args$r13
             if (identical(effect_name, "a1*b1")) {
                 tests <- list(
                     list(outcome = 2, predictors = 1, coef_index = 1, beta = args$a1),
                     list(outcome = 5, predictors = c(1, 2, 3, 4), coef_index = 2, beta = args$b1)
                 )
             } else if (identical(effect_name, "a2*b2")) {
                 tests <- list(
                     list(outcome = 3, predictors = 1, coef_index = 1, beta = args$a2),
                     list(outcome = 5, predictors = c(1, 2, 3, 4), coef_index = 3, beta = args$b2)
                 )
             } else if (identical(effect_name, "a3*b3")) {
                 tests <- list(
                     list(outcome = 4, predictors = 1, coef_index = 1, beta = args$a3),
                     list(outcome = 5, predictors = c(1, 2, 3, 4), coef_index = 4, beta = args$b3)
                 )
             }
         } else if (identical(model_type, "twoserial")) {
             d1_full <- args$d1.full
             sigma <- diag(4)
             sigma[2,1] <- sigma[1,2] <- args$a1
             sigma[3,1] <- sigma[1,3] <- args$a2 + d1_full * args$a1
             sigma[2,3] <- sigma[3,2] <- d1_full + args$a1 * args$a2
             sigma[4,1] <- sigma[1,4] <- cprime + args$a1 * args$b1 + args$a1 * args$b2 * d1_full + args$a2 * args$b2
             sigma[2,4] <- sigma[4,2] <- args$a1 * cprime + args$b1 + args$b2 * d1_full + args$a1 * args$a2 * args$b2
             sigma[3,4] <- sigma[4,3] <- args$a2 * cprime + args$b2 + args$b1 * d1_full + args$a1 * cprime * d1_full
             if (identical(effect_name, "a1*b1")) {
                 tests <- list(
                     list(outcome = 2, predictors = 1, coef_index = 1, beta = args$a1),
                     list(outcome = 4, predictors = c(1, 2, 3), coef_index = 2, beta = args$b1)
                 )
             } else if (identical(effect_name, "a2*b2")) {
                 tests <- list(
                     list(outcome = 3, predictors = c(1, 2), coef_index = 1, beta = args$a2),
                     list(outcome = 4, predictors = c(1, 2, 3), coef_index = 3, beta = args$b2)
                 )
             } else if (identical(effect_name, "a1*d1*b2")) {
                 tests <- list(
                     list(outcome = 2, predictors = 1, coef_index = 1, beta = args$a1),
                     list(outcome = 3, predictors = c(1, 2), coef_index = 2, beta = d1_full),
                     list(outcome = 4, predictors = c(1, 2, 3), coef_index = 3, beta = args$b2)
                 )
             }
         }
         if (is.null(sigma) || is.null(tests))
             return(NA_real_)
         .joint_power_from_sigma(n, sigma, tests)
  }
  .joint.analytic.power <- function(n) {
         se.betas <- .sefun(n, r2s)
         dfs <- n - seq_along(betas) - 1
         pw <- mapply(.joint.path.power, betas, se.betas, dfs)
         prod(pw)
  }
  .joint.analytic.es.power <- function(a_value, n_value = n) {
         r2s_local <- r2s
         betas_local <- betas
         r2s_local[1] <- a_value^2
         r2s_local[2] <- b^2 + cprime^2 + 2 * a_value * b * cprime
         betas_local[1] <- a_value
         se.betas <- .sefun(n_value, r2s_local)
         dfs <- n_value - seq_along(betas_local) - 1
         pw <- mapply(.joint.path.power, betas_local, se.betas, dfs)
         prod(pw)
  }

     switch(test, 
     
     sobel= {
              se.formula<-function(.betas,.se) {
                               cc<-combn(.betas^2, length(.betas)-1, simplify = FALSE)
                               sqrt(sum(sapply(cc,prod) * rev(.se^2)))
                          }
            p.body <- function(n_value = n) {
                n_local <- n
                on.exit(n <<- n_local, add = TRUE)
                n <<- n_value
                eval(p.sobel)
            }
            p.es   <- function(a_value = a, n_value = n) {
                n_local <- n
                a_local <- a
                betas_local <- betas
                r2s_local <- r2s
                on.exit({
                    n <<- n_local
                    a <<- a_local
                    betas <<- betas_local
                    r2s <<- r2s_local
                }, add = TRUE)
                n <<- n_value
                a <<- a_value
                eval(p.es.sobel)
            }

            },
     joint= {
       
            p.body <- function(n_value = n) {
                n_local <- n
                on.exit(n <<- n_local, add = TRUE)
                n <<- n_value
                if (!isTRUE(precise)) {
                    .joint.analytic.power(n)
                } else if (!is.null(args$model_type)) {
                    .joint_complex.power(n = n)
                } else if (length(betas) == 2) {
                    .joint_simple.power(n = n, a = betas[1], b = betas[2], cprime = cprime, r2a = r2s[1], r2y = r2s[2])
                } else {
                    eval(p.joint)
                }
            }
            p.es   <- function(a_value = a, n_value = n) {
                n_local <- n
                a_local <- a
                betas_local <- betas
                r2s_local <- r2s
                on.exit({
                    n <<- n_local
                    a <<- a_local
                    betas <<- betas_local
                    r2s <<- r2s_local
                }, add = TRUE)
                n <<- n_value
                a <<- a_value
                r2s[1] <<- a^2
                r2s[2] <<- b^2+cprime^2+2*a*b*cprime
                betas[1] <<- a
                if (!isTRUE(precise)) {
                    eval(p.es.joint)
                } else if (!is.null(args$model_type)) {
                    .joint_complex.power(n = n)
                } else if (length(betas) == 2) {
                    .joint_simple.power(n = n, a = betas[1], b = betas[2], cprime = cprime, r2a = r2s[1], r2y = r2s[2])
                } else {
                    eval(p.es.joint)
                }
            }

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

    .joint.path.power <- function(beta, se, df) {
      ncp <- beta / se
      switch (alternative,
          two.sided = {
            crit <- qt(1 - sig.level / 2, df = df)
            if (beta >= 0)
              1 - pt(crit, df = df, ncp = ncp)
            else
              pt(-crit, df = df, ncp = ncp)
          },
          one.sided = {
            crit <- qt(1 - sig.level, df = df)
            if (beta >= 0)
              1 - pt(crit, df = df, ncp = ncp)
            else
              pt(-crit, df = df, ncp = ncp)
          }
      )
    }

     # comments and warnings variables
     attribs<-list()
     method<-"pamlj"
     .joint.cache <- new.env(parent = emptyenv())
     .joint.cached_power <- function(n_value) {
        key <- as.character(round(n_value))
        if (!exists(key, envir = .joint.cache, inherits = FALSE)) {
          mark(paste("joint cache: miss at n =", key))
          assign(key, p.body(round(n_value)), envir = .joint.cache)
        } else {
          mark(paste("joint cache: hit at n =", key))
        }
        get(key, envir = .joint.cache, inherits = FALSE)
     }
     .joint.root_fun <- function(n_value) {
        .joint.cached_power(n_value) - power
     }
     .joint.integer_search <- function(n_continuous) {
        mark(paste("joint integer search: start from continuous n =", format(n_continuous, digits = 6)))
        n_low <- max(4, floor(n_continuous))
        n <- n_low
        p_low <- .joint.cached_power(n_low)
        if (is.na(p_low))
          return(list(n = n_low, power = p_low))
        if (p_low >= power) {
          mark(paste("joint integer search: floor n =", n_low, "already reaches target power"))
          repeat {
            if (n_low <= 4)
              break
            n <- n_low - 1
            p_prev <- .joint.cached_power(n)
            if (is.na(p_prev) || p_prev < power) {
              n <- n_low
              mark(paste("joint integer search: selected n =", n_low, "previous power =", format(p_prev, digits = 6), "current power =", format(p_low, digits = 6)))
              return(list(n = n_low, power = p_low, power.previous = p_prev))
            }
            n_low <- n
            p_low <- p_prev
          }
          n <- n_low
          mark(paste("joint integer search: selected minimum feasible n =", n_low))
          return(list(n = n_low, power = p_low))
        }
        n_high <- max(n_low + 1, ceiling(n_continuous))
        n <- n_high
        mark(paste("joint integer search: floor n =", n_low, "below target, searching upward from", n_high))
        p_high <- .joint.cached_power(n_high)
        while (!is.na(p_high) && p_high < power) {
          n_low <- n_high
          p_low <- p_high
          n_high <- n_high + 1
          n <- n_high
          mark(paste("joint integer search: n =", n_low, "power =", format(p_low, digits = 6), "still below target"))
          p_high <- .joint.cached_power(n_high)
        }
        n <- n_high
        mark(paste("joint integer search: selected n =", n_high, "previous power =", format(p_low, digits = 6), "current power =", format(p_high, digits = 6)))
        list(n = n_high, power = p_high, power.previous = p_low)
     }
     .joint.refine_interval <- function(n_start) {
        n0 <- max(10, round(n_start))
        p0 <- p.body(n0)
        if (!is.finite(p0))
          return(c(10, max(12, n0 + 2)))
        diff0 <- p0 - power
        step0 <- max(2, round(abs(diff0) * n0))
        mark(paste("joint n-solver: precise power at analytic start n =", n0, "is", format(p0, digits = 6), "initial step =", step0))
        if (abs(diff0) < .Machine$double.eps^0.5)
          return(c(max(10, n0 - 1), n0 + 1))
        if (diff0 < 0) {
          ll <- n0
          ul <- n0 + step0
          pu <- p.body(ul)
          while (is.finite(pu) && pu < power) {
            ll <- ul
            step0 <- max(2, round(step0 * 1.5))
            ul <- ul + step0
            mark(paste("joint n-solver: expanding upward to", ul))
            pu <- p.body(ul)
          }
          return(c(ll, ul))
        } else {
          ul <- n0
          ll <- max(10, n0 - step0)
          pl <- p.body(ll)
          while (ll > 10 && is.finite(pl) && pl > power) {
            ul <- ll
            step0 <- max(2, round(step0 * 1.5))
            ll <- max(10, ll - step0)
            mark(paste("joint n-solver: expanding downward to", ll))
            pl <- p.body(ll)
          }
          return(c(ll, ul))
        }
     }
     .joint.refine_es_interval <- function(a_start) {
        a0 <- min(max(.00001, a_start), .99999)
        p0 <- p.es(a_value = a0, n_value = n)
        if (!is.finite(p0))
          return(c(.00001, min(.99999, max(.05, a0 + .05))))
        diff0 <- p0 - power
        step0 <- max(.01, abs(diff0) * max(a0, .05))
        mark(paste("joint es-solver: precise power at analytic start a =", format(a0, digits = 6), "is", format(p0, digits = 6), "initial step =", format(step0, digits = 6)))
        if (abs(diff0) < .Machine$double.eps^0.5)
          return(c(max(.00001, a0 - .01), min(.99999, a0 + .01)))
        if (diff0 < 0) {
          ll <- a0
          ul <- min(.99999, a0 + step0)
          pu <- p.es(a_value = ul, n_value = n)
          while (ul < .99999 && is.finite(pu) && pu < power) {
            ll <- ul
            step0 <- max(.01, step0 * 1.5)
            ul <- min(.99999, ul + step0)
            mark(paste("joint es-solver: expanding upward to", format(ul, digits = 6)))
            pu <- p.es(a_value = ul, n_value = n)
          }
          return(c(ll, ul))
        } else {
          ul <- a0
          ll <- max(.00001, a0 - step0)
          pl <- p.es(a_value = ll, n_value = n)
          while (ll > .00001 && is.finite(pl) && pl > power) {
            ul <- ll
            step0 <- max(.01, step0 * 1.5)
            ll <- max(.00001, ll - step0)
            mark(paste("joint es-solver: expanding downward to", format(ll, digits = 6)))
            pl <- p.es(a_value = ll, n_value = n)
          }
          return(c(ll, ul))
        }
     }

     switch(aim, 
            power={
                   if (test=="joint" && isTRUE(precise))
                     mark(paste("joint power: direct evaluation at n =", n))
                   power<-p.body(n)
                  },
            n    ={
                   if (test=="joint" && isTRUE(precise)) {
                      mark(paste("joint n-solver: target power =", power))
                      mark("joint n-solver: starting analytic pilot search")
                      n_start <- try(uniroot(function(n) .joint.analytic.power(n) - power, interval = c(10, 1e10))$root, silent = TRUE)
                      if (!("try-error" %in% class(n_start))) {
                        mark(paste("joint n-solver: analytic pilot found n =", format(n_start, digits = 6)))
                        interval <- .joint.refine_interval(n_start)
                        ll <- interval[1]
                        ul <- interval[2]
                        mark(paste("joint n-solver: simulated root search in [", ll, ",", ul, "]"))
                        n <- try(uniroot(.joint.root_fun, interval = c(ll, ul))$root, silent=F)
                      } else {
                        mark("joint n-solver: analytic pilot failed, using wide simulated search")
                        n <- try(uniroot(.joint.root_fun, interval = c(10, 1e10))$root,silent=F)
                      }
                   } else {
                      n<-try(uniroot(function(n) p.body(n) - power, interval = c(10, 1e10))$root,silent=F)
                   }
                   # if it fails, n should be too small or to large. we test for too small
                   if ("try-error" %in% class(n)) {
                      n<-10
                      pw<-p.body(n)
                      ## if power with 10 is larger than power, we set the minumum n=10
                      if (pw > power) {
                        method="nmin"
                        n<-10
                      } else {
                        ## otherwise, we test for too large
                      n<-1e+07
                      pw<-p.body(n)
                      # is with n=1e+07 we do not reach the required power, we yield and say that n>1e+07
                      if (pw < power) {
                        method="nmax"
                        n<-1e+07
                      }
                      }
                   }
                   if (isTRUE(precise) && !(method %in% c("nmin","nmax")) && test=="joint" && (length(betas)==2 || !is.null(args$model_type))) {
                      mark(paste("joint n-solver: continuous root =", format(n, digits = 6), "starting integer refinement"))
                      check.int <- .joint.integer_search(n)
                      n <- check.int$n
                      if (!is.null(check.int$power))
                        attribs$power <- check.int$power
                      if (!is.null(check.int$power.previous))
                        attribs$power.previous <- check.int$power.previous
                      mark(paste("joint n-solver: final integer n =", n))
                   }
                  },
            es   ={
                   if (aim=="es" && length(betas)>2) stop("es can be estimated only for simple mediation")
                   ## first we test what is the max power we can reach given b
                   x<-seq(0,1,by=.001)
                   if (test=="joint" && isTRUE(precise)) {
                      pow <- sapply(x, function(a) .joint.analytic.es.power(a_value = a, n_value = n))
                   } else {
                      pow<-(sapply(x,function(a) p.es(a_value = a, n_value = n)))
                   }
                   .max<-max(pow)
                   ## if we can go above power, we solve for a
                   if (.max > power) {
                          if (test=="joint" && isTRUE(precise)) {
                              a_start <- uniroot(function(a) .joint.analytic.es.power(a_value = a, n_value = n) - power, interval = c(.00001,x[which.max(pow)] ))$root
                              mark(paste("joint es-solver: analytic pilot found a =", format(a_start, digits = 6)))
                              interval <- .joint.refine_es_interval(a_start)
                              mark(paste("joint es-solver: precise search in [", format(interval[1], digits = 6), ",", format(interval[2], digits = 6), "]"))
                              a <- uniroot(function(a) p.es(a_value = a, n_value = n) - power, interval = interval)$root
                          } else {
                              a<-uniroot(function(a) p.es(a_value = a, n_value = n) - power, interval = c(.00001,x[which.max(pow)] ))$root
                          }
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
                               test="mc",R=1000,L=1000,parallel=FALSE,seed=NULL,...) {

  if (parallel) {
    if (Sys.info()['sysname'] == "Windows") 
                     plan<-future::multisession
    else                 
                     plan<-future::multicore
  
    RNGkind("L'Ecuyer-CMRG")
    future::plan(plan)
    jinfo("MEDIATION MC goes parallel")
  }
  
  if (is.something(seed)) set.seed(seed)
  
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
                 
                   pw<-mean(unlist(foreach::foreach(i = 1:R, 
                                                    .options.future = list(seed = TRUE,
                                                                           globals = structure(c("betas", "se.betas","p.mc","L","sig.level"),add=TRUE) 
                                                                           )) %dofuture%  eval(p.mc )))
                   } else {
                                      
                   pw<-mean(unlist(sapply(1:R, function(i) eval(p.mc) ))) 
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
              mark("MC mediation: Find a reasonable estimation first")
                   check<-pamlj.mediation(a=a,b=b,cprime=cprime,r2a=r2a,r2y=r2y,power=power,sig.level=sig.level, alternative=alternative,test="joint", precise=FALSE)
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
                   check<-pamlj.mediation(n=n,a=NULL,b=b,cprime=cprime,r2a=r2a,r2y=r2y,power=power,sig.level=sig.level, alternative=alternative,test="joint", precise=FALSE)
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
                if (!is.null(args$model_type)) {
                    .joint_complex.power(n = n)
                } else if (length(betas) == 2) {
                    # For simple mediation, estimate power for the actual regression-based joint test.
                    .joint.simple.power(n = n, a = betas[1], b = betas[2], cprime = cprime, r2a = r2s[1], r2y = r2s[2])
                } else {
                    # For longer chains we keep the older pathwise approximation because the full joint
                    # covariance structure is not fully identified by the current inputs alone.
                    se.betas   <- .sefun(n,r2s)
                    dfs        <-  n - seq_along(betas) - 1
                    pw         <-  mapply(.joint.path.power, betas, se.betas, dfs) 
                    prod(pw)
                }
                })
            
            p.es.joint <- quote({
                r2s[1]     <-  a^2
                r2s[2]     <-  b^2+cprime^2+2*a*b*cprime
                betas[1]   <-  a
                if (!is.null(args$model_type)) {
                    .joint_complex.power(n = n)
                } else if (length(betas) == 2) {
                    # Reuse the same simulation-based joint test while solving for the unknown a path.
                    .joint.simple.power(n = n, a = betas[1], b = betas[2], cprime = cprime, r2a = r2s[1], r2y = r2s[2])
                } else {
                    se.betas   <- .sefun(n,r2s)
                    dfs        <-  n - seq_along(betas) - 1
                    pw         <-  mapply(.joint.path.power, betas, se.betas, dfs) 
                    prod(pw)
                }

                })
   
            p.mc  <- quote({               
                # for each beta we draw a random value from its distribution
                pars <- sapply(seq_along(betas),function(j) rnorm(1, betas[j], se.betas[j]))
                ### than we draw a normal distribution for each parameter
                dist <- lapply(seq_along(betas),function(j) rnorm(L, pars[j], se.betas[j]))
                dist <- as.data.frame(do.call(cbind,dist))
                ## and we test the 2.5th (or whatever) quantile of the product of the distributions
                quantile(apply(dist,1,prod), probs=sig.level/2, na.rm = TRUE) > 0
                            
                })
            

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
