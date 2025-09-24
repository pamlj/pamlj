testthat::context("R interface")
tol<-.0001


obj<-pamlj::pamlcorr(es=.1,.interface="R",plot_contour=T,plot_escurve=T,plot_ncurve=T,plot_x="n",plot_y="es",plot_x_from=10,plot_x_to=100)
asum<-summary(obj)
one<-asum[[1]]
ok=1046

testthat::test_that("class ok", {
  testthat::expect_type(asum,"list")
  testthat::expect_type(asum[[1]],"list")
})

testthat::test_that("values ok", {
  testthat::expect_equal(asum[[1]]$n,1046)
  testthat::expect_equal(asum[[2]]$power[2],"50% – 80%")
  
})

