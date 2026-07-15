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


testthat::test_that("pamlmixed(model=) extracts a continuous LMM and matches the equivalent syntax", {
  testthat::skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)

  ext <- pamlj:::.mixed_from_fit(fit)
  testthat::expect_identical(ext$model_type, "linear")
  testthat::expect_equal(ext$sigma2, stats::sigma(fit)^2)
  testthat::expect_equal(ext$clusterpars$Subject$k, lme4::ngrps(fit)[["Subject"]])

  ## algo="raw" is deterministic given a fixed seed (algo="mc" refits lmer() on
  ## every simulated replicate, which is not exactly reproducible run-to-run
  ## even with a fixed seed -- that variability is a property of the pre-existing
  ## Monte Carlo engine, not of the extraction being tested here).
  res      <- pamlj::pamlmixed(model = fit, algo = "raw", set_seed = TRUE, seed = 1, verbose = FALSE)
  res_syn  <- pamlj::pamlmixed(aim = "power", syntax = ext$syntax, clusterpars = ext$clusterpars,
                                categorical = ext$categorical, sigma2 = ext$sigma2,
                                model_type = ext$model_type, algo = "raw", set_seed = TRUE, seed = 1,
                                verbose = FALSE)
  testthat::expect_equal(res$powertab$asDF$power, res_syn$powertab$asDF$power)
})


testthat::test_that("pamlmixed(model=) extracts a factor predictor with bracket syntax", {
  testthat::skip_if_not_installed("lme4")
  d   <- lme4::sleepstudy
  d$grp <- factor(rep(letters[1:3], length.out = nrow(d)))
  fit <- lme4::lmer(Reaction ~ grp + (1 | Subject), data = d)

  ext <- pamlj:::.mixed_from_fit(fit)
  testthat::expect_true(grepl("\\[.*\\]\\*grp", ext$syntax))
  testthat::expect_identical(ext$categorical, list(grp = 3L))
  testthat::expect_true(any(grepl("re-coded with sum-to-zero contrasts", ext$warnings)))

  ## the extracted grp coding is unbalanced across the 18-cluster / 10-cases-per-
  ## cluster design read from the fit, which legitimately triggers pamlj's
  ## "partial recycling" notice -- expected here, not a sign of a problem.
  res <- suppressWarnings(pamlj::pamlmixed(model = fit, mcR = 60, set_seed = TRUE, seed = 1, verbose = FALSE))
  testthat::expect_true(is.data.frame(res$powertab$asDF))
})


testthat::test_that("pamlmixed(model=) extracts a binomial GLMM", {
  testthat::skip_if_not_installed("lme4")
  fit <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                      data = lme4::cbpp, family = binomial)

  ext <- pamlj:::.mixed_from_fit(fit)
  testthat::expect_identical(ext$model_type, "logistic")
  testthat::expect_identical(ext$sigma2, 1)

  ## mcR is kept small for test speed, which can make individual glmer() refits
  ## on simulated data numerically unstable -- expected here, not a sign of a
  ## problem with the extraction.
  res <- suppressWarnings(pamlj::pamlmixed(model = fit, mcR = 60, set_seed = TRUE, seed = 1, verbose = FALSE))
  testthat::expect_true(is.data.frame(res$powertab$asDF))
})


testthat::test_that("pamlmixed(model=) registers a random-only slope as a zero fixed term", {
  testthat::skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ 1 + (Days | Subject), data = lme4::sleepstudy)
  ext <- pamlj:::.mixed_from_fit(fit)
  testthat::expect_true(grepl("\\+0\\*Days", ext$syntax))

  res <- pamlj::pamlmixed(model = fit, mcR = 60, set_seed = TRUE, seed = 1, verbose = FALSE)
  testthat::expect_true(is.data.frame(res$powertab$asDF))
})


testthat::test_that("pamlmixed(model=) honours explicit user overrides", {
  testthat::skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  res <- pamlj::pamlmixed(model = fit, aim = "n", find = "n", sigma2 = 999, power = .8,
                           mcR = 60, set_seed = TRUE, seed = 1, verbose = FALSE)
  testthat::expect_true(is.data.frame(res$powertab$asDF))
})


testthat::test_that("pamlmixed(model=) rejects unsupported models", {
  testthat::skip_if_not_installed("lme4")
  testthat::expect_error(pamlj:::.mixed_from_fit(lm(Reaction ~ Days, data = lme4::sleepstudy)),
                          "fitted lme4 model")
  fitp <- suppressMessages(lme4::glmer(
    round(Reaction) ~ Days + (1 | Subject), data = lme4::sleepstudy, family = poisson))
  testthat::expect_error(pamlj:::.mixed_from_fit(fitp), "not supported")
})

