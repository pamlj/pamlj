testthat::context("glm")

tol <- 1e-4


testthat::test_that("beta mode returns numeric power", {
  obj <- pamlj::pamlglm(
    aim = "power",
    mode = "beta",
    es = 0.25,
    r2 = 0.12,
    df_model = 3,
    n = 50
  )

  tab <- obj$powertab$asDF
  testthat::expect_true(is.data.frame(tab))
  testthat::expect_true(is.finite(tab$power))
  testthat::expect_type(tab$power, "double")
  testthat::expect_true(tab$power > 0)
  testthat::expect_true(tab$power < 1)
})


testthat::test_that("peta mode returns required N", {
  obj <- pamlj::pamlglm(
    aim = "n",
    mode = "peta",
    es = 0.10,
    df_model = 4,
    df_effect = 2,
    power = 0.80
  )

  tab <- obj$powertab$asDF
  testthat::expect_true(is.data.frame(tab))
  testthat::expect_true(is.finite(tab$n))
  testthat::expect_true(tab$n >= 6)
  testthat::expect_equal(tab$df1, 2)
})


testthat::test_that("eta mode returns effect size", {
  obj <- pamlj::pamlglm(
    aim = "es",
    mode = "eta",
    df_model = 3,
    df_effect = 2,
    r2 = 0.30,
    power = 0.80,
    n = 80
  )

  tab <- obj$powertab$asDF
  testthat::expect_true(is.data.frame(tab))
  testthat::expect_true(is.finite(tab$es))
  testthat::expect_type(tab$es, "double")
  testthat::expect_true(tab$es > 0)
  testthat::expect_true(tab$es < 1)
})
