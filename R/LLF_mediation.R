## ----------------------------------------------------------------------------
## .mediation.implied_cor() : model-implied correlation/covariance matrix from path coeffs
##
## Computes  Sigma = F (I - A)^{-1} S (I - A)^{-T} F^T   (McArdle-McDonald RAM).
##
##   (I - A)^{-1} = I + A + A^2 + ...  performs the path tracing automatically:
##   the (i,j) entry sums the products of coefficients along every directed
##   path from j to i. Sandwiching with S adds the "common-cause" (double-headed
##   arrow) contributions. This generalises Wright's rules to any recursive OR
##   non-recursive model, with or without latent variables.
## ----------------------------------------------------------------------------

.mediation.implied_cor <- function(A, S = NULL, F = NULL, standardize = TRUE) {

  A <- as.matrix(A)
  p <- nrow(A)
  if (ncol(A) != p) stop("A must be square.")

  if (is.null(F)) F <- diag(p)          # default: all variables observed
  if (is.null(S)) S <- matrix(0, p, p)  # default: no fixed (co)variances
  S <- as.matrix(S)

  ## (I - A)^{-1}  -- the path-tracing operator
  IA_inv <- solve(diag(p) - A)

  if (standardize) {
    ## Find the diagonal of S (disturbance / exogenous variances) so that every
    ## variable has unit variance, i.e. the result is a CORRELATION matrix.
    ## Sigma is linear in S, so diag(M S M^T) = 1 is a linear system in those
    ## unknown variances v:   P v = b,   with  P_ik = M_ik^2.
    M  <- IA_inv
    P  <- M^2
    b  <- 1 - diag(M %*% S %*% t(M))     # contribution of the FIXED part of S
    v  <- solve(P, b)                    # required variances
    diag(S) <- diag(S) + v
  }

  Sigma <- F %*% IA_inv %*% S %*% t(IA_inv) %*% t(F)

  ## tidy up tiny numerical asymmetry
  Sigma <- (Sigma + t(Sigma)) / 2
  dimnames(Sigma) <- list(rownames(A), rownames(A))
  Sigma
}

## ============================================================================
##  .mediation.indirect_paths() : enumerate every simple directed path of >= 2
##  edges in a RAM path matrix A (A[i, j] = path FROM j TO i). Each path is an
##  indirect (mediated) effect, returned as a vector of node indices.
## ============================================================================

.mediation.indirect_paths <- function(A) {
  A <- as.matrix(A)
  p <- nrow(A)
  succ <- lapply(seq_len(p), function(u) which(A[, u] != 0))   # u points to ...
  paths <- list()
  dfs <- function(node, chain) {
    for (nx in succ[[node]]) {
      if (nx %in% chain) next                 # keep the path simple (no loops)
      newchain <- c(chain, nx)
      if (length(newchain) >= 3)              # >= 2 edges -> an indirect effect
        paths[[length(paths) + 1L]] <<- newchain
      dfs(nx, newchain)
    }
  }
  for (s in seq_len(p)) dfs(s, s)
  paths
}

## ============================================================================
##  .mediation.r2() : R^2 of one endogenous variable's equation, recovered from
##  the model-implied correlation matrix Sigma. The predictors are the variables
##  that point to `outcome` in the path matrix A (A[outcome, ] != 0).
## ============================================================================

.mediation.r2 <- function(Sigma, A, outcome) {
  P <- which(A[outcome, ] != 0)
  if (!length(P)) return(0)
  as.numeric(crossprod(Sigma[P, outcome], solve(Sigma[P, P, drop = FALSE], Sigma[P, outcome])))
}

## ============================================================================
##  .mediation.coef_se() : every regression in the model, read off Sigma at N.
## ----------------------------------------------------------------------------
##  For each endogenous variable i (a row of A with incoming paths) its
##  predictors are the columns j with A[i, j] != 0. The population OLS solution
##  on the model-implied matrix Sigma is
##        beta = Sigma[P,P]^{-1} Sigma[P,i],
##  with sampling (co)variance  sigma2_resid * Sigma[P,P]^{-1} / (N - k - 1),
##  sigma2_resid = Sigma[i,i] - Sigma[i,P] beta. These are exactly the estimates
##  and standard errors lm() returns on data with covariance Sigma at sample
##  size N -- the single source of truth for both the Sobel and the joint test.
##
##  Returns matrices `coef` and `se` (zero / NA off the model's edges) and a
##  per-equation residual-df vector `df`.
##
##  `free` is the logical pattern of ESTIMATED paths (predictors per equation).
##  It is NOT the same as the non-zero pattern of A: a path may be freely
##  estimated yet have a population value of exactly 0 (e.g. the direct effect
##  c' in a mediation model is always in the fitted regression Y ~ X + M, even
##  when c' = 0). Such a predictor must stay in its equation, because it changes
##  the residual df AND inflates the SE of the other coefficients. Defaults to
##  A != 0 when not supplied.
## ============================================================================

.mediation.coef_se <- function(A, Sigma, N, free = NULL) {
  A     <- as.matrix(A)
  Sigma <- as.matrix(Sigma)
  vars  <- rownames(A); if (is.null(vars)) vars <- paste0("v", seq_len(nrow(A)))
  p     <- nrow(A)
  if (is.null(free)) free <- (A != 0)

  coef <- matrix(0,        p, p, dimnames = list(vars, vars))
  se   <- matrix(NA_real_, p, p, dimnames = list(vars, vars))
  df   <- setNames(rep(NA_real_, p), vars)

  for (i in seq_len(p)) {
    P <- which(free[i, ])                         # estimated predictors of variable i
    if (!length(P)) next
    k <- length(P); df[i] <- N - k - 1
    if (df[i] <= 0) stop("N too small for the equation of '", vars[i], "'.")
    Sinv <- solve(Sigma[P, P, drop = FALSE])
    beta <- as.numeric(Sinv %*% Sigma[P, i])
    s2   <- Sigma[i, i] - sum(Sigma[P, i] * beta) # residual variance
    coef[i, P] <- beta
    se[i, P]   <- sqrt(s2 * diag(Sinv) / df[i])
  }
  list(coef = coef, se = se, df = df)
}

## ---- power of a single coefficient test from its noncentrality --------------
## z-test (Sobel: the product effect is one normal statistic) and t-test (joint:
## each edge is an ordinary regression t-test on N - k - 1 df).

.mediation.z_power <- function(ncp, alpha, alternative = "two.sided") {
  ncp <- abs(ncp)
  if (alternative == "two.sided") {
    zc <- qnorm(1 - alpha / 2)
    1 - pnorm(zc - ncp) + pnorm(-zc - ncp)
  } else {
    zc <- qnorm(1 - alpha)
    1 - pnorm(zc - ncp)
  }
}

.mediation.t_power <- function(ncp, df, alpha, alternative = "two.sided") {
  ncp <- abs(ncp)
  if (alternative == "two.sided") {
    tc <- qt(1 - alpha / 2, df)
    pt(tc, df, ncp, lower.tail = FALSE) + pt(-tc, df, ncp)
  } else {
    tc <- qt(1 - alpha, df)
    pt(tc, df, ncp, lower.tail = FALSE)
  }
}

.mediation.path_edges <- function(chain) {
  list(
    from = chain[-length(chain)],
    to   = chain[-1]
  )
}

.mediation.edge_values <- function(mat, edges) {
  mapply(function(i, j) mat[i, j], edges$to, edges$from)
}

.mediation.indirect_effect <- function(A, chain) {
  prod(.mediation.edge_values(A, .mediation.path_edges(chain)))
}

.mediation.default_free <- function(A, chain) {
  free <- A != 0

  ## The outcome regression includes the direct X -> Y path even when c' = 0.
  ## This keeps the b-path SE aligned with lm(Y ~ X + M).
  free[chain[length(chain)], chain[1]] <- TRUE
  free
}

.mediation.target_path <- function(A, target = NULL) {
  paths <- .mediation.indirect_paths(A)
  if (!length(paths)) stop("A contains no indirect (>= 2 edge) path.")

  if (is.null(target)) {
    if (length(paths) > 1)
      stop("The model has several indirect paths; specify `target`.")
    return(paths[[1]])
  }

  vars <- rownames(A)
  if (is.character(target)) match(target, vars) else as.integer(target)
}

.mediation.safe_implied_cor <- function(A, S = NULL) {
  tryCatch(
    suppressWarnings(.mediation.implied_cor(A, S)),
    error = function(e) NULL
  )
}

.mediation.rescale_effect <- function(A, chain, es) {
  current_es <- .mediation.indirect_effect(A, chain)
  if (!is.finite(current_es) || abs(current_es) == 0 || abs(es - current_es) <= 1e-9)
    return(A)

  first_to   <- chain[2]
  first_from <- chain[1]
  A[first_to, first_from] <- A[first_to, first_from] * (es / current_es)
  A
}

.mediation.solve_n <- function(power_at_n, target_power) {
  root <- try(
    uniroot(function(nn) power_at_n(nn) - target_power,
            interval = c(10, 1e10))$root,
    silent = TRUE
  )

  if (!inherits(root, "try-error"))
    return(list(n = ceiling(root), method = "pamlj"))

  if (isTRUE(power_at_n(10) >= target_power))
    return(list(n = 10, method = "nmin"))

  list(n = 1e+07, method = "nmax")
}

.mediation.solve_n_grid <- function(power_at_n, target_power, n_min = 10,
                                    n_max = 10000, n_seed = NULL) {
  ## Bootstrap searches are seeded from a joint-significance estimate when one
  ## is available. The old bounded doubling search remains as a fallback.

  solve_from_seed <- function(seed_n) {
    seed_n <- round(seed_n)
    if (!is.finite(seed_n)) return(NULL)
    seed_n <- max(n_min, min(n_max, seed_n))

    seed_power <- power_at_n(seed_n)
    if (!is.finite(seed_power)) return(NULL)

    if (isTRUE(seed_power >= target_power)) {
      if (isTRUE(power_at_n(n_min) >= target_power))
        return(list(n = n_min, method = "nmin"))

      low <- n_min
      high <- seed_n
      while ((high - low) > 1) {
        mid <- floor((low + high) / 2)
        if (isTRUE(power_at_n(mid) >= target_power)) high <- mid else low <- mid
      }
      return(list(n = high, method = "pamlj"))
    }

    low <- seed_n
    high <- seed_n
    repeat {
      high <- min(n_max, max(high + 1, ceiling(high * 1.5)))
      high_power <- power_at_n(high)
      if (isTRUE(high_power >= target_power)) break
      if (high >= n_max) return(list(n = n_max, method = "nmax"))
      low <- high
    }

    while ((high - low) > 1) {
      mid <- floor((low + high) / 2)
      if (isTRUE(power_at_n(mid) >= target_power)) high <- mid else low <- mid
    }

    list(n = high, method = "pamlj")
  }

  if (!is.null(n_seed)) {
    seeded <- solve_from_seed(n_seed)
    if (!is.null(seeded)) return(seeded)
  }

  low <- n_min
  low_power <- power_at_n(low)
  if (isTRUE(low_power >= target_power))
    return(list(n = low, method = "nmin"))

  high <- low
  repeat {
    high <- min(n_max, max(high + 1, high * 2))
    high_power <- power_at_n(high)
    if (isTRUE(high_power >= target_power)) break
    if (high >= n_max) return(list(n = n_max, method = "nmax"))
  }

  while ((high - low) > 1) {
    mid <- floor((low + high) / 2)
    if (isTRUE(power_at_n(mid) >= target_power)) high <- mid else low <- mid
  }

  list(n = high, method = "pamlj")
}

.mediation.solve_mde <- function(A, chain, S, n, target_power, power_for,
                                 seed_mag = NULL, vary_edge = NULL) {
  ## `vary_edge` = c(to, from) indices of the coefficient to resize. Defaults to
  ## the first edge of `chain` (the a-path). The caller picks any edge (a/b/d) to vary while `chain`
  ## stays the indirect path whose power is driven to target_power.
  if (is.null(vary_edge)) vary_edge <- c(chain[2], chain[1])
  first_to   <- vary_edge[1]
  first_from <- vary_edge[2]
  sign_first <- if (A[first_to, first_from] != 0) sign(A[first_to, first_from]) else 1

  ## The search probes infeasible coefficient values (negative residual variance,
  ## non-PD implied matrix); the power engines already return NA there, so the
  ## resulting NaN warnings are expected noise and are muffled.
  eval_pf <- function(trial) suppressWarnings(power_for(trial, .mediation.safe_implied_cor(trial, S), n))

  eval_mag <- function(magnitude) {
    trial <- A
    trial[first_to, first_from] <- sign_first * magnitude
    eval_pf(trial)
  }

  solve_from_seed <- function(seed_mag) {
    seed_mag <- abs(seed_mag)
    if (!is.finite(seed_mag)) return(NULL)
    seed_mag <- max(1e-5, min(.999, seed_mag))

    seed_power <- eval_mag(seed_mag)
    if (!is.finite(seed_power)) return(NULL)

    if (isTRUE(seed_power >= target_power)) {
      low <- 1e-5
      high <- seed_mag
      low_power <- eval_mag(low)
      if (isTRUE(low_power >= target_power)) {
        A[first_to, first_from] <- sign_first * low
        return(list(A = A, power = low_power, method = "powmin"))
      }
    } else {
      ## Power vs coefficient is unimodal: it rises, peaks, then FALLS back toward
      ## alpha as the coefficient approaches 1 and the standardized model
      ## degenerates (the mediator becomes collinear with X, so the other edge's
      ## standard error explodes). When the target lies above the peak we must
      ## report the PEAK -- the best achievable power -- not keep walking to the
      ## boundary where power has collapsed. Track the strongest magnitude seen
      ## while expanding and return it if we reach the boundary without bracketing.
      best_mag   <- seed_mag
      best_power <- seed_power
      low <- seed_mag
      high <- seed_mag
      repeat {
        low <- high
        high <- min(.999, max(high + 1e-3, high * 1.5))
        high_power <- eval_mag(high)
        if (is.finite(high_power) && high_power > best_power) {
          best_power <- high_power
          best_mag   <- high
        }
        if (isTRUE(high_power >= target_power)) break
        if (high >= .999) {
          A[first_to, first_from] <- sign_first * best_mag
          return(list(A = A, power = best_power, method = "powmax"))
        }
      }
    }

    root <- uniroot(function(magnitude) eval_mag(magnitude) - target_power,
                    interval = c(low, high))$root
    A[first_to, first_from] <- sign_first * root
    list(A = A, power = target_power, method = "pamlj")
  }

  if (!is.null(seed_mag)) {
    seeded <- solve_from_seed(seed_mag)
    if (!is.null(seeded)) return(seeded)
  }

  grid <- seq(1e-3, .999, by = .001)
  grid_power <- vapply(grid, eval_mag, numeric(1))

  usable <- is.finite(grid_power)
  if (!any(usable))
    stop("Could not evaluate power for any magnitude of the target path at N=", n)

  max_index <- which(usable)[which.max(grid_power[usable])]
  if (grid_power[max_index] <= target_power) {
    A[first_to, first_from] <- sign_first * grid[max_index]
    return(list(A = A, power = grid_power[max_index], method = "powmax"))
  }

  root <- uniroot(function(magnitude) eval_mag(magnitude) - target_power,
                  interval = c(1e-5, grid[max_index]))$root

  A[first_to, first_from] <- sign_first * root
  list(A = A, power = target_power, method = "pamlj")
}

## ----------------------------------------------------------------------------
## Equal-components minimum detectable effect (fallback for an unreachable target).
##
## Resizing a single edge cannot exceed the peak imposed by the OTHER, fixed edges
## (as one edge -> 1 the mediator becomes collinear with its predictor and the
## remaining edges can no longer be estimated, so power collapses). When the target
## power is above that peak there is no single-coefficient answer. Growing ALL the
## edges of the path together removes the fixed bottleneck: power then rises
## monotonically and any target below 1 is reachable. This solves for the common
## magnitude `t` (each edge keeps its own sign) that delivers the target power and
## reports the resulting balanced indirect effect (t^k for a k-edge path).
## ----------------------------------------------------------------------------
.mediation.solve_equal_components <- function(A, chain, S, n, target_power, power_for) {
  edges <- .mediation.path_edges(chain)
  k     <- length(edges$to)
  signs <- vapply(seq_len(k), function(e) {
    s <- sign(A[edges$to[e], edges$from[e]]); if (s == 0) 1 else s
  }, numeric(1))

  set_A <- function(t) {
    trial <- A
    for (e in seq_len(k)) trial[edges$to[e], edges$from[e]] <- signs[e] * t
    trial
  }
  eval_mag <- function(t)
    suppressWarnings(power_for(set_A(t), .mediation.safe_implied_cor(set_A(t), S), n))

  low   <- 1e-3
  low_p <- eval_mag(low)
  if (!is.finite(low_p)) return(NULL)
  if (isTRUE(low_p >= target_power))
    return(list(A = set_A(low), power = low_p, method = "balanced"))

  ## expand upward until the (monotone increasing) power brackets the target
  lo <- low
  hi <- low
  repeat {
    hi <- min(.999, max(hi + 1e-3, hi * 1.5))
    hi_p <- eval_mag(hi)
    if (isTRUE(hi_p >= target_power)) break
    if (is.finite(hi_p)) lo <- hi
    if (hi >= .999)
      ## even with every edge maximal the target is out of reach: report the best
      return(list(A = set_A(hi), power = hi_p, method = "powmax"))
  }
  root <- uniroot(function(t) eval_mag(t) - target_power, interval = c(lo, hi))$root
  list(A = set_A(root), power = target_power, method = "balanced")
}

.mediation.result <- function(A, Sigma, chain, n, power, sig.level, method) {
  es         <- .mediation.indirect_effect(A, chain)
  first_to   <- chain[2]
  first_from <- chain[1]
  outcome    <- chain[length(chain)]
  exposure   <- chain[1]
  a          <- A[first_to, first_from]
  b          <- if (a != 0) es / a else 0

  list(n = round(n, digits = 0), a = a, b = b, es = es,
       cprime = A[outcome, exposure],
       r2a = .mediation.r2(Sigma, A, first_to),
       r2y = .mediation.r2(Sigma, A, outcome),
       sig.level = sig.level, power = power, method = method)
}

## ============================================================================
## Monte Carlo confidence-interval power  (test = "parametric" / "simulation")
## ----------------------------------------------------------------------------
## Both methods estimate the power a researcher obtains when the indirect effect
## is judged significant by a confidence interval on the PRODUCT of the path
## coefficients that excludes zero -- i.e. the bootstrap-CI workflow. They differ
## only in how each simulated study's coefficient estimates and standard errors
## are obtained:
##
##   "parametric"  draws them analytically from the (known) sampling distribution
##                 of the OLS estimates -- no data, no model fitting.
##   "simulation"  generates one dataset from Sigma and fits the path regressions
##                 once (fast normal-equations OLS) to obtain them.
##
## In both cases each study's interval is built by the Monte Carlo method
## (MacKinnon et al. 2004; Preacher & Selig 2012; Schoemann, Boulton & Short
## 2017): draw L values of each coefficient from its sampling distribution,
## multiply along the path, and take the percentile interval. This reproduces
## bootstrap-CI power WITHOUT the nested resample-and-refit that makes a true
## bootstrap prohibitively slow. R = number of simulated studies; L = Monte Carlo
## draws per interval.
## ============================================================================

.mediation.sim_data <- function(Sigma, N) {
  ## Draw standardized multivariate normal data with covariance/correlation
  ## Sigma. Sigma already contains the path model implied by A and S.
  vars <- colnames(Sigma)
  if (is.null(vars)) vars <- paste0("v", seq_len(ncol(Sigma)))
  z <- matrix(stats::rnorm(N * ncol(Sigma)), N, ncol(Sigma)) %*% chol(Sigma)
  colnames(z) <- vars
  z
}

## ---- shared Monte Carlo CI decision -----------------------------------------
## Given, for each simulated study (rows) and each path edge (columns), the
## coefficient estimate `bhat` and its estimated standard error `sehat`, return
## the proportion of studies whose percentile MC interval for the product of the
## edges excludes zero. Fully vectorised: the L*R Monte Carlo draws for one edge
## are generated in a single rnorm() call. A percentile interval [a/2, 1-a/2]
## excludes zero exactly when the share of negative product draws is below a/2
## (interval entirely positive) or above 1-a/2 (entirely negative), so no sort
## or quantile call is needed -- a column mean of a sign comparison suffices.

.mediation.mc_ci_power <- function(bhat, sehat, L, alpha) {
  R <- nrow(bhat)
  E <- ncol(bhat)
  prod_draws <- matrix(1, L, R)
  for (e in seq_len(E)) {
    draws <- matrix(stats::rnorm(L * R, mean = rep(bhat[, e], each = L),
                                 sd = rep(sehat[, e], each = L)), L, R)
    prod_draws <- prod_draws * draws
  }
  neg <- colMeans(prod_draws < 0)
  mean(neg < alpha / 2 | neg > 1 - alpha / 2)
}

.mediation.mc_power_parametric <- function(A, Sigma, N, chain, free, alpha = .05,
                                           R = 1000, L = 2000, seed = NULL, ...) {
  ## Analytic Monte Carlo CI: skip data and model fitting entirely. The OLS
  ## estimate and standard error a real study would report are read off the
  ## model-implied Sigma at sample size N by .mediation.coef_se().
  if (!is.finite(N) || is.null(Sigma) || any(!is.finite(Sigma))) return(NA_real_)
  coef_info <- tryCatch(.mediation.coef_se(A, Sigma, round(N), free),
                        error = function(e) NULL)
  if (is.null(coef_info)) return(NA_real_)

  edges <- .mediation.path_edges(chain)
  beta  <- .mediation.edge_values(coef_info$coef, edges)
  se    <- .mediation.edge_values(coef_info$se, edges)
  df    <- coef_info$df[edges$to]
  if (any(!is.finite(beta)) || any(!is.finite(se) | se <= 0) ||
      any(!is.finite(df) | df <= 0)) return(NA_real_)

  if (!is.null(seed)) set.seed(seed)

  E <- length(beta)
  ## Each simulated study's coefficient estimate is normal around the population
  ## value with the analytic SD `se`; its ESTIMATED SE scales with the sample
  ## residual SD, whose square is chi-square distributed. Coefficient and
  ## residual variance are independent under Gaussian OLS, and path edges live in
  ## different equations, so every column is drawn independently.
  bhat  <- matrix(0, R, E)
  sehat <- matrix(0, R, E)
  for (e in seq_len(E)) {
    bhat[, e]  <- stats::rnorm(R, beta[e], se[e])
    sehat[, e] <- se[e] * sqrt(stats::rchisq(R, df[e]) / df[e])
  }

  .mediation.mc_ci_power(bhat, sehat, L, alpha)
}

.mediation.mc_power_simulation <- function(A, Sigma, N, chain, free, alpha = .05,
                                           R = 1000, L = 2000, seed = NULL,
                                           parallel = FALSE, ...) {
  ## Simulation Monte Carlo CI: generate R datasets from Sigma and fit each ONCE
  ## via normal-equations OLS (no formula parsing, no lm()), then build the MC
  ## interval from the fitted estimates and SEs.
  if (!is.finite(N) || is.null(Sigma) || any(!is.finite(Sigma))) return(NA_real_)
  Nr    <- round(N)
  edges <- .mediation.path_edges(chain)
  E     <- length(edges$to)

  ## Each distinct outcome equation is fitted once per dataset; map every path
  ## edge to its equation and the position of its predictor within that fit.
  outcomes <- unique(edges$to)
  preds    <- lapply(outcomes, function(i) which(free[i, ]))
  if (any(vapply(preds, function(P) Nr - length(P) - 1 <= 0, logical(1))))
    return(NA_real_)

  chol_S <- tryCatch(chol(Sigma), error = function(e) NULL)
  if (is.null(chol_S)) return(NA_real_)
  p <- ncol(Sigma)

  ## one simulated study -> c(estimate per edge, estimated SE per edge)
  one <- function(i) {
    z <- matrix(stats::rnorm(Nr * p), Nr, p) %*% chol_S
    b <- numeric(E); s <- numeric(E)
    for (oi in seq_along(outcomes)) {
      i_out  <- outcomes[oi]
      P      <- preds[[oi]]
      X      <- cbind(1, z[, P, drop = FALSE])
      y      <- z[, i_out]
      XtXinv <- tryCatch(solve(crossprod(X)), error = function(e) NULL)
      if (is.null(XtXinv)) { b[edges$to == i_out] <- NA; next }
      betas  <- XtXinv %*% crossprod(X, y)
      resid  <- y - X %*% betas
      s2     <- sum(resid^2) / (Nr - length(P) - 1)
      sej    <- sqrt(s2 * diag(XtXinv))
      for (h in which(edges$to == i_out)) {
        pos  <- match(edges$from[h], P) + 1L     # +1 for the intercept column
        b[h] <- betas[pos]
        s[h] <- sej[pos]
      }
    }
    c(b, s)
  }

  if (!is.null(seed)) set.seed(seed)

  if (isTRUE(parallel)) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    if (Sys.info()[['sysname']] == "Windows") future::plan(future::multisession)
    else future::plan(future::multicore)
    mat <- t(simplify2array(
      foreach::foreach(i = seq_len(R), .options.future = list(seed = TRUE)) %dofuture% one(i)))
  } else {
    mat <- t(vapply(seq_len(R), one, numeric(2 * E)))
  }

  bhat  <- mat[, seq_len(E), drop = FALSE]
  sehat <- mat[, E + seq_len(E), drop = FALSE]
  ok    <- stats::complete.cases(bhat, sehat) & apply(sehat > 0, 1, all)
  if (!any(ok)) return(NA_real_)
  .mediation.mc_ci_power(bhat[ok, , drop = FALSE], sehat[ok, , drop = FALSE], L, alpha)
}

## ============================================================================
##  .mediation.power_dispatch() : build the power function for a given test.
## ----------------------------------------------------------------------------
##  Returns a closure power(A, Sigma, N, chain, free) selecting the right engine
##  (Sobel family / joint via .mediation.path_power, or the Monte Carlo CI
##  methods). Used by pamlj.mediation() and by the complex-model MDE orchestrator
##  so both share one definition of "power of an indirect path".
## ============================================================================

.mediation.power_dispatch <- function(test, sig.level = .05, alternative = "two.sided",
                                      R = 1000, L = 2000, parallel = FALSE, seed = NULL) {
  function(A, Sigma, N, chain, free = NULL) {
    if (is.null(free)) free <- .mediation.default_free(A, chain)
    if (test == "parametric")
      .mediation.mc_power_parametric(A, Sigma, N, chain, free, sig.level,
                                     R = R, L = L, seed = seed)
    else if (test == "simulation")
      .mediation.mc_power_simulation(A, Sigma, N, chain, free, sig.level,
                                     R = R, L = L, seed = seed, parallel = parallel)
    else
      .mediation.path_power(A, Sigma, N, chain, test, sig.level, alternative, free)
  }
}

## ============================================================================
##  .mediation.path_power() : power of detecting ONE indirect effect.
## ----------------------------------------------------------------------------
##  `chain` is the indirect path as a vector of node indices (as produced by
##  .mediation.indirect_paths()); its edges are chain[k] -> chain[k+1]. Power is
##  computed two ways, sharing the coefficients and SEs from .mediation.coef_se:
##
##    "sobel" / "aroian" / "goodman"
##        the product effect ab... is a single statistic; its SE is the
##        (first-order delta-method) Sobel SE, and power follows from the normal
##        approximation z = effect / SE. "aroian"/"goodman" add/subtract the
##        second-order term (two-edge paths only).
##
##    "joint"
##        joint-significance: the indirect effect is declared only when EVERY
##        edge is individually significant, so the power is the PRODUCT of the
##        edges' marginal (noncentral-t) powers. The edges live in different
##        regression equations and are treated as independent -- the standard
##        joint-significance approximation.
##
##  Returns NA (rather than erroring) when the model/df at this N is infeasible,
##  so the root-finding / grid callers can simply skip those points.
## ============================================================================

.mediation.path_power <- function(A, Sigma, N, chain, test = "sobel",
                                  alpha = 0.05, alternative = "two.sided", free = NULL) {
  if (!is.finite(N) || is.null(Sigma) || any(!is.finite(Sigma))) return(NA_real_)
  coef_info <- tryCatch(.mediation.coef_se(A, Sigma, N, free), error = function(e) NULL)
  if (is.null(coef_info)) return(NA_real_)

  edges <- .mediation.path_edges(chain)
  th    <- .mediation.edge_values(coef_info$coef, edges)
  se    <- .mediation.edge_values(coef_info$se, edges)
  df    <- coef_info$df[edges$to]
  if (any(!is.finite(th)) || any(!is.finite(se) | se <= 0) ||
      any(!is.finite(df) | df <= 0)) return(NA_real_)

  if (test == "joint")
    return(prod(mapply(function(t, s, d)
                       .mediation.t_power(t / s, d, alpha, alternative), th, se, df)))

  ## Sobel family: delta-method variance of the product effect.
  est <- prod(th)
  v   <- sum((est / th)^2 * se^2)
  if (length(th) == 2L && test == "aroian")  v <- v + prod(se^2)
  if (length(th) == 2L && test == "goodman") v <- v - prod(se^2)
  .mediation.z_power(est / sqrt(v), alpha, alternative)
}

## ============================================================================
##  pamlj.mediation()  -- power, required N, or minimum detectable effect (MDE)
##  for a mediation model described by its RAM path matrix.
## ----------------------------------------------------------------------------
##  The model is passed in, not rebuilt here:
##    A      directed-path matrix, A[i, j] = standardized path FROM j TO i. Its
##           nonzero pattern is the model structure; its values are the coeffs.
##    Sigma  the model-implied (standardized) correlation matrix. If NULL it is
##           derived with .mediation.implied_cor(A, S).
##    S      fixed residual (co)variances among variables (e.g. correlated
##           mediators); only used when Sigma must be rebuilt after a coefficient
##           is changed (the `es` aim and the es-resize for plots). NULL = none.
##    Both A, Sigma (and S) are built in .checkdata.<model>() and injected by
##    .powervector.mediation().
##
##  Power of one mediated effect is delegated to .mediation.path_power() for the
##  Sobel family ("sobel"/"aroian"/"goodman") and joint-significance ("joint"),
##  or to the Monte Carlo CI engines .mediation.mc_power_parametric() /
##  .mediation.mc_power_simulation() for the bootstrap-CI methods ("parametric"
##  and "simulation").
##
##  target  node-index or node-name chain selecting which indirect path to size
##          / report. If NULL the model must contain exactly one indirect path.
##  Exactly one of `n`, `power`, `es` is NULL; that is the quantity solved for.
## ============================================================================

pamlj.mediation <- function(A, Sigma = NULL, S = NULL, free = NULL, n = NULL, power = NULL,
                            es = NULL, target = NULL, sig.level = .05,
                            alternative = "two.sided", test = "sobel",
                            R = 1000, L = 2000, parallel = FALSE, seed = NULL, ...) {

  if (!test %in% c("sobel", "aroian", "goodman", "joint", "parametric", "simulation"))
    stop("pamlj.mediation() supports the Sobel test family ('sobel', 'aroian', ",
         "'goodman'), joint-significance ('joint'), and the Monte Carlo CI ",
         "methods ('parametric', 'simulation').")

  A <- as.matrix(A)
  aim   <- if (is.null(n)) "n" else if (is.null(power)) "power" else "es"
  is_mc <- test %in% c("parametric", "simulation")
  chain <- .mediation.target_path(A, target)

  if (is.null(free)) free <- .mediation.default_free(A, chain)

  engine    <- .mediation.power_dispatch(test, sig.level, alternative,
                                         R = R, L = L, parallel = parallel, seed = seed)
  power_for <- function(Amat, Sig, n_val) engine(Amat, Sig, n_val, chain, free)

  ## Monte Carlo power is a *random* function of N / the coefficient, so a plain
  ## root-find or grid search over it is unreliable: uniroot can fail to bracket
  ## the target (the noisy endpoints land on the same side) and the solved value
  ## drifts run to run. We solve instead on a frozen objective using common random
  ## numbers -- the engine re-seeds before every evaluation, so the same seed makes
  ## the simulated power a smooth, deterministic function of its argument. The
  ## user's own seed is used when supplied; otherwise a fixed internal seed. Only
  ## the search uses this; the direct power aim keeps the user's seed semantics.
  solve_seed   <- if (!is.null(seed)) seed else 20240101L
  solve_engine <- if (is_mc)
      .mediation.power_dispatch(test, sig.level, alternative,
                                R = R, L = L, parallel = parallel, seed = solve_seed)
    else engine
  power_solve  <- function(Amat, Sig, n_val) solve_engine(Amat, Sig, n_val, chain, free)

  if (is.null(Sigma)) Sigma <- .mediation.safe_implied_cor(A, S)

  if (!is.null(es) && aim != "es") {
    A <- .mediation.rescale_effect(A, chain, es)
    Sigma <- .mediation.safe_implied_cor(A, S)
  }

  method <- "pamlj"

  switch(aim,
    power = {
      power <- power_for(A, Sigma, n)
    },
    n = {
      if (is_mc) {
        joint_seed <- .mediation.solve_n(
          function(nn) .mediation.path_power(A, Sigma, nn, chain, "joint",
                                             sig.level, alternative, free),
          power
        )$n
        solved <- .mediation.solve_n_grid(function(nn) power_solve(A, Sigma, nn),
                                          power, n_seed = joint_seed)
      } else {
        solved <- .mediation.solve_n(function(nn) power_solve(A, Sigma, nn), power)
      }
      n <- solved$n
      method <- solved$method
    },
    es = {
      if (is_mc) {
        joint_seed <- .mediation.solve_mde(
          A, chain, S, n, power,
          function(Amat, Sig, n_val) {
            .mediation.path_power(Amat, Sig, n_val, chain, "joint",
                                  sig.level, alternative, free)
          }
        )
        seed_mag <- abs(joint_seed$A[chain[2], chain[1]])
        solved <- .mediation.solve_mde(A, chain, S, n, power, power_solve,
                                       seed_mag = seed_mag)
      } else {
        solved <- .mediation.solve_mde(A, chain, S, n, power, power_solve)
      }
      ## Resizing the first edge alone could not reach the target (it sits above
      ## the achievable peak). Fall back to growing every edge of the path together,
      ## which removes the fixed-edge bottleneck and can reach any target below 1.
      if (identical(solved$method, "powmax")) {
        balanced <- .mediation.solve_equal_components(A, chain, S, n, power, power_solve)
        if (!is.null(balanced)) solved <- balanced
      }
      A <- solved$A
      power <- solved$power
      method <- solved$method
      Sigma <- .mediation.safe_implied_cor(A, S)
    })

  .mediation.result(A, Sigma, chain, n, power, sig.level, method)
}
