testthat::context("mixed")

tol <- 1e-4


testthat::test_that("mixed syntax with invalid expand cluster warns and returns results", {
  m <- "
y~0*1+.65*x+a*.3*z+(1*1+5.8*x|cluster)+(1*1|cluster2)
bet:x|cluster
within:z|cluster
test: a
expand: cluster2d
"

  testthat::expect_warning(
    obj <- pamlj::pamlmixed(
      aim = "n",
      find = "k",
      syntax = m,
      clusterpars = list(cluster = c(n = 10, k = 20), cluster2 = c(n = 2, k = 10)),
      algo = "raw",
      sigma2 = 30
    )
  )

  tab <- obj$powertab$asDF
  testthat::expect_true(is.data.frame(tab))
  testthat::expect_true(all(is.finite(tab$power)))
  testthat::expect_true(all(is.finite(tab$k)))
})


testthat::test_that("mixed syntax with valid expand cluster works", {
  m <- "
y~0*1+.65*x+a*.3*z+(1*1+5.8*x|cluster)+(1*1|cluster2)
bet:x|cluster
within:z|cluster
test: a
expand: cluster2
"

  obj <- pamlj::pamlmixed(
    aim = "n",
    find = "k",
    syntax = m,
    clusterpars = list(cluster = c(n = 10, k = 20), cluster2 = c(n = 2, k = 10)),
    algo = "raw",
    sigma2 = 30
  )

  tab <- obj$powertab$asDF
  testthat::expect_true(is.data.frame(tab))
  testthat::expect_true(all(is.finite(tab$power)))
  testthat::expect_true(all(is.finite(tab$k)))
})

