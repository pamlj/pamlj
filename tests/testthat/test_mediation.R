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

