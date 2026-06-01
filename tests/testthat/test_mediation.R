testthat::context("mediation")

tol <- 1e-4

testthat::test_that("joint mediation returns finite sample sizes", {
  approx <- pamlj:::pamlj.mediation(
    a = 0.363,
    b = 0.387,
    cprime = 0,
    power = 0.80,
    test = "joint",
    precise = FALSE,
    R = 5
  )

  precise <- pamlj:::pamlj.mediation(
    a = 0.363,
    b = 0.387,
    cprime = 0,
    power = 0.80,
    test = "joint",
    precise = TRUE,
    R = 20
  )

  testthat::expect_true(is.finite(approx$n))
  testthat::expect_true(is.finite(precise$n))
  testthat::expect_true(approx$n >= 10)
  testthat::expect_true(precise$n >= 10)
  testthat::expect_true(precise$n >= approx$n - 2)
})

testthat::test_that("joint mediation power is bounded", {
  precise <- pamlj:::pamlj.mediation(
    a = 0.363,
    b = 0.387,
    cprime = 0,
    n = 74,
    test = "joint",
    precise = TRUE,
    R = 20
  )

  testthat::expect_true(is.finite(precise$power))
  testthat::expect_true(precise$power > 0)
  testthat::expect_true(precise$power < 1)
})

testthat::test_that("mc mediation uses joint pilot and returns finite n", {
  mc <- pamlj:::pamlj.mediation.mc(
    a = 0.363,
    b = 0.387,
    cprime = 0,
    power = 0.80,
    test = "mc",
    R = 20,
    L = 50,
    seed = 12
  )

  testthat::expect_true(is.finite(mc$n))
  testthat::expect_true(mc$n >= 10)
  testthat::expect_true(is.finite(mc$power))
})

testthat::test_that("pamlmed interface returns finite mediation results", {
  obj <- pamlj::pamlmed(
    a = 0.363,
    b = 0.387,
    cprime = 0,
    power = 0.80,
    test = "joint",
    mcR = 20,
    parallel = FALSE,
    .interface = "R"
  )

  tab <- obj$powertab$asDF
  testthat::expect_true(is.data.frame(tab))
  testthat::expect_true(is.finite(tab$n))
  testthat::expect_true(tab$n >= 10)
  testthat::expect_true(is.finite(tab$es))
  testthat::expect_true(is.finite(tab$power))
})
