testthat::context("correlation")
tol <- .0001


obj <- pamlj::pamlcorr(es = .1, .interface = "R", plot_contour = T, plot_escurve = T, plot_ncurve = T, plot_x = "n", plot_y = "es", plot_x_from = 10, plot_x_to = 100)

testthat::test_that("values ok", {
    testthat::expect_equal(obj$powertab$asDF$n, 1046)
    testthat::expect_equal(obj$powerbyes$asDF$es[4], "ρ > 0.1111")
})

plots <- plots(obj)

testthat::test_that("plots ok", {
    testthat::expect_true(ggplot2::is.ggplot(plots$powerCustom))
})

testthat::expect_warning(
    obj <- pamlj::pamlcorr(es = .0001, .interface = "R", plot_x = "n", plot_y = "es", plot_x_from = 10, plot_x_to = 100)
)

ok <- 10747773
testthat::test_that("values ok", {
    testthat::expect_equal(obj$powertab$asDF$n, ok)
    testthat::expect_equal(obj$powerbyes$asDF$es[4], "ρ > 0.001075")
})

obj <- pamlj::pamlcorr(aim = "power", es = .1, n = 50, .interface = "R")

ok <- 0.1063704
testthat::test_that("power ok", {
    testthat::expect_equal(obj$powertab$asDF$power, ok)
    testthat::expect_equal(obj$powerbyes$asDF$es[4], "ρ > 0.479")
})


obj <- pamlj::pamlcorr(aim = "es", power = .99, n = 50, .interface = "R")

ok <- 0.5511182
testthat::test_that("power ok", {
    testthat::expect_equal(obj$powertab$asDF$es, ok)
    testthat::expect_equal(obj$powerbyes$asDF$es[4], "ρ > 0.479")
})
