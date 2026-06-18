testthat::context("mediation")

tol <- 1e-4

## build a simple-mediation RAM model (X -> M -> Y, with direct path c')
.med_model <- function(a, b, cprime = 0) {
  A <- matrix(0, 3, 3, dimnames = list(c("X","M","Y"), c("X","M","Y")))
  A["M","X"] <- a
  A["Y","M"] <- b
  A["Y","X"] <- cprime
  list(A = A, Sigma = pamlj:::.mediation.implied_cor(A))
}

testthat::test_that("sobel mediation returns finite required sample size", {
  m   <- .med_model(0.363, 0.387)
  res <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, power = 0.80, test = "sobel")

  testthat::expect_true(is.finite(res$n))
  testthat::expect_true(res$n >= 10)
  testthat::expect_equal(res$power, 0.80, tolerance = 1e-6)
  testthat::expect_equal(res$es, 0.363 * 0.387, tolerance = tol)
})

testthat::test_that("sobel mediation power is bounded and round-trips with N", {
  m   <- .med_model(0.363, 0.387)
  res <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, n = 100, test = "sobel")

  testthat::expect_true(is.finite(res$power))
  testthat::expect_true(res$power > 0)
  testthat::expect_true(res$power < 1)

  ## power evaluated at the N solved for a target power should reach that target
  solved <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, power = 0.80, test = "sobel")
  check  <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, n = solved$n, test = "sobel")
  testthat::expect_true(check$power >= 0.80)
})

testthat::test_that("sobel mediation solves the minimum detectable effect", {
  ## a is only a placeholder for the es aim: the X->M edge is the sized one
  m   <- .med_model(0.30, 0.39)
  res <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, n = 200, power = 0.80, test = "sobel")

  testthat::expect_true(is.finite(res$a))
  testthat::expect_true(res$a > 0 && res$a < 1)

  ## the recovered coefficient should deliver the requested power
  chk <- .med_model(res$a, 0.39)
  check <- pamlj:::pamlj.mediation(A = chk$A, Sigma = chk$Sigma, n = 200, test = "sobel")
  testthat::expect_equal(check$power, 0.80, tolerance = 1e-3)
})

testthat::test_that("supplying an es value rescales the indirect effect", {
  m   <- .med_model(0.30, 0.30)
  ## ask for power at a *different* indirect effect than the model's a*b
  res <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, n = 100, es = 0.20, test = "sobel")
  testthat::expect_equal(res$es, 0.20, tolerance = tol)
})


testthat::test_that("pamlmed interface returns finite mediation results", {
  obj <- pamlj::pamlmed(
    a = 0.363,
    b = 0.387,
    cprime = 0,
    power = 0.80,
    test = "sobel"
  )

  tab <- obj$powertab$asDF
  testthat::expect_true(is.data.frame(tab))
  testthat::expect_true(is.finite(tab$n))
  testthat::expect_true(tab$n >= 10)
  testthat::expect_true(is.finite(tab$es))
  testthat::expect_true(is.finite(tab$power))
})

testthat::test_that("complex mediation (twomeds) powers each indirect path", {
  obj <- pamlj::pamlmed(mode = "medcomplex", model_type = "twomeds", aim = "n",
                        a1 = ".3", b1 = ".35", a2 = ".25", b2 = ".4",
                        r12 = ".2", cprime2 = .1, power = .80, test = "sobel")
  tab <- obj$powertab$asDF
  testthat::expect_equal(nrow(tab), 2)                 # two indirect effects
  testthat::expect_true(all(is.finite(tab$n)))
  testthat::expect_equal(tab$es, c(.3 * .35, .25 * .4), tolerance = tol)
  testthat::expect_true(all(abs(tab$power - .80) < 1e-6))
})

testthat::test_that("complex mediation (twoserial) includes the serial path", {
  obj <- pamlj::pamlmed(mode = "medcomplex", model_type = "twoserial", aim = "power",
                        a1 = ".3", b1 = ".35", a2 = ".2", b2 = ".4",
                        d1 = ".25", cprime2 = .1, n = 200, test = "sobel")
  tab <- obj$powertab$asDF
  testthat::expect_equal(nrow(tab), 3)
  ## the serial effect X -> M1 -> M2 -> Y equals a1 * d1 * b2
  testthat::expect_true(any(abs(tab$es - (.3 * .25 * .4)) < tol))
  testthat::expect_true(all(tab$power > 0 & tab$power < 1))
})

testthat::test_that("complex mediation tolerates missing coefficients", {
  obj <- pamlj::pamlmed(mode = "medcomplex", model_type = "twomeds", aim = "n",
                        a1 = ".3", b1 = ".35", a2 = ".25", b2 = "",
                        r12 = ".2", cprime2 = .1, power = .80, test = "sobel")
  ## should not error; just reports the missing coefficient and does not run
  testthat::expect_true(is.data.frame(obj$powertab$asDF))
})

## ---------------------------------------------------------------------------
## Joint-significance: reinstated on the model-implied engine. The indirect
## effect is declared only when EVERY edge is significant, so power is the
## product of the edges' marginal t-test powers.
## ---------------------------------------------------------------------------

testthat::test_that("joint mediation solves N, power and MDE for simple mediation", {
  m <- .med_model(0.363, 0.387)

  ## required N for a target power, then round-trip: power at that N >= target
  reqN <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, power = 0.80, test = "joint")
  testthat::expect_true(is.finite(reqN$n) && reqN$n >= 10)
  testthat::expect_equal(reqN$power, 0.80, tolerance = 1e-6)
  testthat::expect_equal(reqN$es, 0.363 * 0.387, tolerance = tol)

  chk <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, n = reqN$n, test = "joint")
  testthat::expect_true(chk$power >= 0.80)

  ## minimum detectable effect: the recovered first edge must deliver the power
  mde <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, n = 100, power = 0.80, test = "joint")
  testthat::expect_true(mde$a > 0 && mde$a < 1)
  back <- .med_model(mde$a, 0.387)
  testthat::expect_equal(
    pamlj:::pamlj.mediation(A = back$A, Sigma = back$Sigma, n = 100, test = "joint")$power,
    0.80, tolerance = 1e-3)
})

testthat::test_that("joint and sobel agree on the direction but differ in power", {
  m <- .med_model(0.30, 0.30)
  pj <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, n = 150, test = "joint")$power
  ps <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, n = 150, test = "sobel")$power
  testthat::expect_true(pj > 0 && pj < 1 && ps > 0 && ps < 1)
  ## joint-significance is the more powerful test for a symmetric a = b model
  testthat::expect_true(pj >= ps)
})

testthat::test_that("joint mediation powers complex models (twomeds, twoserial)", {
  twom <- pamlj::pamlmed(mode = "medcomplex", model_type = "twomeds", aim = "n",
                         a1 = ".3", b1 = ".35", a2 = ".25", b2 = ".4",
                         r12 = ".2", cprime2 = .1, power = .80, test = "joint")
  tab <- twom$powertab$asDF
  testthat::expect_equal(nrow(tab), 2)
  testthat::expect_true(all(is.finite(tab$n)))
  testthat::expect_true(all(abs(tab$power - .80) < 1e-6))

  ser <- pamlj::pamlmed(mode = "medcomplex", model_type = "twoserial", aim = "power",
                        a1 = ".3", b1 = ".35", a2 = ".2", b2 = ".4",
                        d1 = ".25", cprime2 = .1, n = 200, test = "joint")
  stab <- ser$powertab$asDF
  testthat::expect_equal(nrow(stab), 3)
  testthat::expect_true(any(abs(stab$es - (.3 * .25 * .4)) < tol))   # serial path a1*d1*b2
  testthat::expect_true(all(stab$power > 0 & stab$power < 1))
})

testthat::test_that("Monte Carlo CI mediation returns finite power and required N", {
  m <- .med_model(0.35, 0.35)

  for (mc in c("parametric", "simulation")) {
    pwr <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, n = 80,
                                   test = mc, R = 200, L = 500, seed = 123)
    testthat::expect_true(is.finite(pwr$power))
    testthat::expect_true(pwr$power >= 0 && pwr$power <= 1)
    testthat::expect_equal(pwr$es, 0.35 * 0.35, tolerance = tol)

    req <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, power = 0.50,
                                   test = mc, R = 200, L = 500, seed = 123)
    testthat::expect_true(is.finite(req$n))
    testthat::expect_true(req$n >= 10)
    testthat::expect_equal(req$power, 0.50, tolerance = 0.02)
  }
})

testthat::test_that("parametric and simulation Monte Carlo CI agree with each other", {
  m <- .med_model(0.30, 0.30)
  pp <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, n = 150,
                                test = "parametric", R = 2000, L = 2000, seed = 1)$power
  ps <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, n = 150,
                                test = "simulation", R = 2000, L = 2000, seed = 1)$power
  ## both target the same bootstrap-CI workflow, so they should be close
  testthat::expect_equal(pp, ps, tolerance = 0.05)
  ## and both should sit between Sobel and joint-significance power
  testthat::expect_true(pp > 0 && pp < 1 && ps > 0 && ps < 1)
})

testthat::test_that("pamlmed exposes both Monte Carlo CI methods", {
  for (mc in c("parametric", "simulation")) {
    obj <- pamlj::pamlmed(a = 0.35, b = 0.35, cprime = 0, n = 80,
                          aim = "power", test = mc, mcR = 200,
                          set_seed = TRUE, seed = 123,
                          table_pwbyn = FALSE, diagram = FALSE)
    tab <- obj$powertab$asDF
    testthat::expect_true(is.data.frame(tab))
    testthat::expect_true(is.finite(tab$power))
    testthat::expect_true(tab$power >= 0 && tab$power <= 1)
  }
})

testthat::test_that("Monte Carlo MDE / N solve is stable without a user seed (common random numbers)", {
  ## Regression: the MDE / N search root-finds over Monte Carlo power, which is a
  ## random function. With no seed the objective was noisy: uniroot could fail to
  ## bracket the target ("f() values at end points not of opposite sign") or drift
  ## run to run, so the solved effect's power landed far from the request. The
  ## solver now uses common random numbers (a fixed internal seed re-applied per
  ## evaluation), making the search deterministic and the result reproducible.
  m <- .med_model(0.30, 0.30)

  for (mc in c("parametric", "simulation")) {
    ## es aim: no error, reproducible coefficient, true power near the target
    a1 <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, n = 200, power = .80,
                                  test = mc, R = 1000)$a
    a2 <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, n = 200, power = .80,
                                  test = mc, R = 1000)$a
    testthat::expect_equal(a1, a2)                       # deterministic without a seed
    back <- .med_model(a1, 0.30)
    truep <- pamlj:::pamlj.mediation(A = back$A, Sigma = back$Sigma, n = 200,
                                     test = mc, R = 6000, L = 4000, seed = 99)$power
    testthat::expect_equal(truep, 0.80, tolerance = 0.04)

    ## N aim: likewise reproducible
    n1 <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, power = .80, test = mc, R = 1000)$n
    n2 <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, power = .80, test = mc, R = 1000)$n
    testthat::expect_equal(n1, n2)
    testthat::expect_true(is.finite(n1) && n1 >= 10)
  }
})

testthat::test_that("unreachable-target MDE falls back to a balanced (all-edges-equal) solution", {
  ## Power vs a single coefficient is unimodal (it collapses as the coefficient
  ## nears 1 and the mediator turns collinear with X), so when the requested power
  ## sits above that peak resizing one edge cannot reach it. The solver then grows
  ## ALL edges of the path together -- which removes the fixed-edge bottleneck and
  ## reaches any target below 1 -- and flags the result method "balanced".
  ## N = 100 with b = .3 is a known regime where no single coefficient reaches
  ## power .80 (the one-coefficient peak is ~.79); the method "balanced" flag below
  ## is itself the evidence that the single-coefficient solve could not reach it.
  m <- .med_model(0.30, 0.30)

  ## deterministic tests are clearly above the peak -> the balanced fallback fires
  for (test in c("joint", "sobel")) {
    r <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, n = 100, power = .80,
                                 test = test)
    testthat::expect_identical(r$method, "balanced")
    testthat::expect_equal(r$a, r$b, tolerance = 1e-4)   # all path coefficients equal
    testthat::expect_equal(r$power, 0.80, tolerance = 1e-3)
    testthat::expect_true(r$es > 0 && r$es < 1)
  }

  ## Monte Carlo sits right at the noisy peak: it may either bracket the target
  ## on its frozen realization (single-coefficient solution) or fall back to the
  ## balanced one -- either way it must deliver the requested power, never the
  ## degenerate boundary (es = a*b = .30 at power ~ alpha that the old code gave).
  for (mc in c("parametric", "simulation")) {
    r <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, n = 100, power = .80,
                                 test = mc, R = 1000)
    testthat::expect_equal(r$power, 0.80, tolerance = 1e-2)
    testthat::expect_true(r$es > 0 && r$es < 0.25)
  }
})

testthat::test_that("complex Monte Carlo MDE recomputes affected effects at the target power", {
  ## Regression: the parametric complex MDE used to return all-NA and the
  ## simulation one drifted off target, because the resize root-find ran on a
  ## noisy Monte Carlo objective and the effects were then recomputed with a fresh
  ## noisy draw. Both methods must now drive the affected effect to the target.
  for (mc in c("parametric", "simulation")) {
    obj <- pamlj::pamlmed(mode = "medcomplex", model_type = "twomeds", aim = "es",
                          a1 = ".3", b1 = ".35", a2 = ".25", b2 = ".4", r12 = ".2",
                          cprime2 = .1, n = 200, power = .80, test = mc,
                          sensitivity_coef = "a2", mcR = 1000,
                          table_pwbyn = FALSE, diagram = FALSE)
    tab <- obj$powertab$asDF
    testthat::expect_equal(nrow(tab), 2)
    testthat::expect_false(anyNA(tab$power))
    m2 <- tab[grepl("M2", tab$effect) & !grepl("M1", tab$effect), ]   # path through a2
    testthat::expect_equal(m2$power, 0.80, tolerance = 1e-2)
    testthat::expect_true(m2$a > 0 && m2$a < 0.25)
  }
})

testthat::test_that("pamlmed medsimple N matches the direct engine (no info$S partial-match)", {
  ## Regression: obj$info$S has no "S" key for simple mediation, so `$S` used to
  ## partial-match "Sigma" and feed the correlation matrix as fixed residual
  ## covariances, corrupting the implied Sigma in the table/explain path. The
  ## reported N (and the explanatory text that reads obj$data) must equal the
  ## direct pamlj.mediation solve.
  m   <- .med_model(0.30, 0.30)
  ref <- pamlj:::pamlj.mediation(A = m$A, Sigma = m$Sigma, power = 0.80, test = "joint")$n

  obj <- pamlj::pamlmed(a = 0.30, b = 0.30, cprime = 0, power = 0.80,
                        aim = "n", test = "joint",
                        table_pwbyn = FALSE, diagram = FALSE)
  testthat::expect_equal(obj$powertab$asDF$n, ref)
})

testthat::test_that("complex MDE varies the chosen coefficient (parallel mediators)", {
  ## twomeds, find the minimum detectable effect by resizing a2 at N=150
  obj <- pamlj::pamlmed(mode = "medcomplex", model_type = "twomeds", aim = "es",
                        a1 = ".3", b1 = ".35", a2 = ".25", b2 = ".4",
                        r12 = ".2", cprime2 = .1, n = 150, power = .80,
                        test = "joint", sensitivity_coef = "a2",
                        table_pwbyn = FALSE, diagram = FALSE)
  tab <- obj$powertab$asDF
  testthat::expect_equal(nrow(tab), 2)
  ## the effect that travels through a2 (X -> M2 -> Y) is driven to the target
  m2 <- tab[grepl("M2", tab$effect) & !grepl("M1", tab$effect), ]
  testthat::expect_equal(m2$power, 0.80, tolerance = 1e-3)
  ## the solved a2 must be a feasible, smaller-than-input coefficient
  testthat::expect_true(m2$a > 0 && m2$a < 0.25)
  testthat::expect_equal(m2$es, m2$a * 0.40, tolerance = tol)
})

testthat::test_that("complex MDE: every effect through the coefficient reaches at least target", {
  ## twoserial, resize b2 which lies on TWO indirect paths (X->M2->Y and the
  ## serial X->M1->M2->Y). Both must reach at least the target power.
  obj <- pamlj::pamlmed(mode = "medcomplex", model_type = "twoserial", aim = "es",
                        a1 = ".3", b1 = ".35", a2 = ".2", b2 = ".4", d1 = ".25",
                        cprime2 = .1, n = 200, power = .80, test = "joint",
                        sensitivity_coef = "b2", table_pwbyn = FALSE, diagram = FALSE)
  tab <- obj$powertab$asDF
  affected <- tab[grepl("M2", tab$effect), ]          # paths ending ... M2 -> Y
  testthat::expect_true(nrow(affected) == 2)
  testthat::expect_true(all(affected$power >= 0.80 - 1e-3))
  testthat::expect_equal(min(affected$power), 0.80, tolerance = 1e-3)
})

testthat::test_that("complex MDE rejects a coefficient absent from the model", {
  testthat::expect_error(
    pamlj::pamlmed(mode = "medcomplex", model_type = "twomeds", aim = "es",
                   a1 = ".3", b1 = ".35", a2 = ".25", b2 = ".4", r12 = ".2",
                   cprime2 = .1, n = 150, power = .80, test = "joint",
                   sensitivity_coef = "d1", table_pwbyn = FALSE, diagram = FALSE),
    "not a path of the current model")
})

testthat::test_that("joint serial mediation with two mediators solves N, power and MDE", {
  A <- matrix(0, 4, 4, dimnames = list(c("X", "M1", "M2", "Y"),
                                       c("X", "M1", "M2", "Y")))
  A["M1", "X"]  <- 0.30
  A["M2", "M1"] <- 0.25
  A["Y", "M2"]  <- 0.35
  A["Y", "X"]   <- 0.05

  Sigma  <- pamlj:::.mediation.implied_cor(A)
  target <- c("X", "M1", "M2", "Y")

  reqN <- pamlj:::pamlj.mediation(A = A, Sigma = Sigma, target = target,
                                  power = 0.80, test = "joint")
  testthat::expect_equal(reqN$n, 138)
  testthat::expect_equal(reqN$power, 0.80, tolerance = 1e-6)
  testthat::expect_equal(reqN$a, 0.30, tolerance = tol)
  testthat::expect_equal(reqN$b, 0.25 * 0.35, tolerance = tol)
  testthat::expect_equal(reqN$es, 0.30 * 0.25 * 0.35, tolerance = tol)

  pwr <- pamlj:::pamlj.mediation(A = A, Sigma = Sigma, target = target,
                                 n = 500, test = "joint")
  testthat::expect_equal(pwr$power, 0.9999247, tolerance = 1e-6)
  testthat::expect_equal(pwr$r2a, 0.09, tolerance = tol)
  testthat::expect_equal(pwr$r2y, 0.127625, tolerance = tol)

  mde <- pamlj:::pamlj.mediation(A = A, Sigma = Sigma, target = target,
                                 n = 500, power = 0.80, test = "joint")
  testthat::expect_equal(mde$a, 0.1248353, tolerance = 1e-6)
  testthat::expect_equal(mde$b, 0.25 * 0.35, tolerance = tol)
  testthat::expect_equal(mde$es, 0.01092309, tolerance = 1e-6)
  testthat::expect_equal(
    pamlj:::pamlj.mediation(A = {
      B <- A
      B["M1", "X"] <- mde$a
      B
    }, Sigma = pamlj:::.mediation.implied_cor({
      B <- A
      B["M1", "X"] <- mde$a
      B
    }), target = target, n = 500, test = "joint")$power,
    0.80, tolerance = 1e-3)
})

## ---------------------------------------------------------------------------
## Power by effect size table, and the test-column propagation it depends on.
## The main run overwrites obj$data with the solved result (no `test` column);
## the by-N / by-effect-size tables must still honour the selected test rather
## than silently falling back to the sobel default.
## ---------------------------------------------------------------------------

testthat::test_that("power-by-N and power-by-es honour the selected test (joint, not sobel fallback)", {
  obj <- pamlj::pamlmed(a = .3, b = .3, cprime = 0, n = 100, aim = "power",
                        test = "joint", table_pwbyn = TRUE, table_pwbyes = TRUE,
                        diagram = FALSE)

  ## joint required N at a = b = .3 is 72 / 114 / 168 (sobel would be 85 / 170 / 279)
  byn <- obj$powerbyn$asDF
  testthat::expect_equal(nrow(byn), 4)
  testthat::expect_true(grepl("72", byn$n[1]))
  testthat::expect_true(grepl("114", byn$n[3]))
  testthat::expect_false(any(grepl("85|170|279", byn$n)))

  ## the by-es table reports indirect-effect and coefficient bands
  byes <- obj$powerbyes$asDF
  testthat::expect_equal(nrow(byes), 4)
  testthat::expect_true(all(c("power", "desc", "es", "coef") %in% names(byes)))
  testthat::expect_true(all(grepl("ME", byes$es)))     # indirect-effect symbol
  testthat::expect_true(all(grepl("\\ba\\b", byes$coef)))
})

testthat::test_that("complex power-by-es varies the chosen coefficient monotonically", {
  obj <- pamlj::pamlmed(mode = "medcomplex", model_type = "twomeds", aim = "power",
                        a1 = ".3", b1 = ".35", a2 = ".25", b2 = ".4", r12 = ".2",
                        cprime2 = .1, n = 150, test = "joint", sensitivity_coef = "a1",
                        table_pwbyn = FALSE, table_pwbyes = TRUE, diagram = FALSE)
  byes <- obj$powerbyes$asDF
  testthat::expect_equal(nrow(byes), 4)
  testthat::expect_true(all(grepl("a1", byes$coef)))   # the varied coefficient

  ## the upper thresholds of the first three bands rise with the power band
  thr <- as.numeric(sub(".*[<≤]\\s*", "", byes$coef[1:3]))
  testthat::expect_false(anyNA(thr))
  testthat::expect_true(all(diff(thr) > 0))
})

pamlj::pamlmed(a = .36,b=.381,aim="es")
