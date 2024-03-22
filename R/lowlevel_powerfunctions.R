## these are low level power functions, modified from various sources to fit the need of the module

pamlj.glm <- function(u=NULL,v=NULL,f2=NULL,power=NULL,alpha=NULL,df_model=NULL,gpower=TRUE, tails="two") {
  

    if (tails=="one" ) {
         if ( is.something(alpha) )
               alpha <- alpha * 2
         else 
             stop("The required power parameter cannot be computed for one-tailed tests")
    }
    if (is.null(df_model))
         stop("df_model must be defined")
  
    ncp <-function(f2,u,v) {
       if (gpower)
          f2* (df_model + v+ 1)
        else
          f2* (u + v+ 1)
       }

p.body <- quote({
        lambda <- ncp(f2 , u, v)
        pf(qf(alpha, u, v, lower = FALSE), u, v, lambda, lower = FALSE)
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
    else if (is.null(alpha)) 
        alpha <- uniroot(function(sig.level) eval(p.body) - 
            power, c(1e-10, 1 - 1e-10))$root
    else stop("internal error")
    n <- df_model+ v + 1
    structure(list(u = u, v = v, f2 = f2, alpha = alpha, 
        power = power, n = n), class = "pamlj_power")

}

