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
  testthat::expect_identical(ext$categorical$grp$levels, 3L)
  testthat::expect_identical(ext$categorical$grp$coding, "custom")
  ## grp has default (treatment) contrasts in the fit, so the extracted matrix
  ## should be contr.treatment(3)
  testthat::expect_equal(pamlj:::.mixed_decode_contrasts(ext$categorical$grp$contrasts, 3),
                          unname(stats::contr.treatment(3)))
  testthat::expect_true(any(grepl("exact contrast coding", ext$warnings)))

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
  ## overriding sigma2 to an extreme, unrealistic value together with the small
  ## mcR used here for test speed can make individual lmer() refits on simulated
  ## data numerically unstable -- expected here, not a sign of a problem.
  res <- suppressWarnings(pamlj::pamlmixed(model = fit, aim = "n", find = "n", sigma2 = 999, power = .8,
                           mcR = 60, set_seed = TRUE, seed = 1, verbose = FALSE))
  testthat::expect_true(is.data.frame(res$powertab$asDF))
})


testthat::test_that("categorical= supports named coding schemes and custom contrast matrices", {
  m <- "y~1*1+[.3,-.2]*grp+(1*1|cluster)"
  cl <- list(cluster = c(n = 10, k = 20))

  ## every named scheme runs and yields a k-1 = 2 coefficient design (no error)
  for (coding in c("deviation", "simple", "dummy", "difference", "helmert", "repeated", "polynomial")) {
    res <- suppressWarnings(pamlj::pamlmixed(
      aim = "power", syntax = m, clusterpars = cl,
      categorical = list(grp = list(levels = 3, coding = coding)),
      algo = "raw", verbose = FALSE))
    testthat::expect_true(is.finite(res$powertab$asDF$power), info = coding)
  }

  ## bare level count (backward-compatible shorthand) is equivalent to explicit "deviation"
  res_short <- suppressWarnings(pamlj::pamlmixed(
    aim = "power", syntax = m, clusterpars = cl, categorical = list(grp = 3),
    algo = "raw", verbose = FALSE))
  res_dev <- suppressWarnings(pamlj::pamlmixed(
    aim = "power", syntax = m, clusterpars = cl,
    categorical = list(grp = list(levels = 3, coding = "deviation")),
    algo = "raw", verbose = FALSE))
  testthat::expect_equal(res_short$powertab$asDF$power, res_dev$powertab$asDF$power)

  ## an explicit custom contrast matrix (here, contr.sum) matches "deviation"
  res_custom <- suppressWarnings(pamlj::pamlmixed(
    aim = "power", syntax = m, clusterpars = cl,
    categorical = list(grp = list(levels = 3, coding = "custom", contrasts = contr.sum(3))),
    algo = "raw", verbose = FALSE))
  testthat::expect_equal(res_custom$powertab$asDF$power, res_dev$powertab$asDF$power)

  ## "custom" without a matrix fails validation (surfaced as NA results, per this
  ## engine's convention of turning checkdata errors into warnings + empty tables)
  res_bad <- suppressWarnings(pamlj::pamlmixed(
    aim = "power", syntax = m, clusterpars = cl,
    categorical = list(grp = list(levels = 3, coding = "custom")),
    algo = "raw", verbose = FALSE))
  testthat::expect_true(is.na(res_bad$powertab$asDF$power))
})


testthat::test_that("cor()/cov() correlate two random-effect terms via their symbols", {
  cl <- list(cluster = c(n = 10, k = 20))

  ## cov(a,b)=<covariance> is used verbatim as the off-diagonal entry
  res_cov <- pamlj::pamlmixed(
    aim = "power", syntax = "y~1*1+.5*x+(a*1*1+b*.3*x|cluster)\ncov(a,b)=.15",
    clusterpars = cl, algo = "raw", verbose = FALSE, .info = TRUE)
  cv <- res_cov$info$model$random$cluster$covariances
  testthat::expect_length(cv, 1)
  testthat::expect_equal(cv[[1]]$cov, .15)

  ## cor(a,b)=<correlation> is converted to a covariance using the two variances
  res_cor <- pamlj::pamlmixed(
    aim = "power", syntax = "y~1*1+.5*x+(a*1*1+b*.3*x|cluster)\ncor(a,b)=.4",
    clusterpars = cl, algo = "raw", verbose = FALSE, .info = TRUE)
  cv2 <- res_cor$info$model$random$cluster$covariances
  testthat::expect_equal(cv2[[1]]$cov, .4 * sqrt(1 * .3))

  ## an implied correlation > 1 is rejected (with a warning) and the covariance is dropped
  testthat::expect_warning(
    res_bad <- pamlj::pamlmixed(
      aim = "power", syntax = "y~1*1+.5*x+(a*1*1+b*1*x|cluster)\ncov(a,b)=5",
      clusterpars = cl, algo = "raw", verbose = FALSE, .info = TRUE),
    "exceeds 1")
  testthat::expect_length(res_bad$info$model$random$cluster$covariances, 0)

  ## symbols from two different clusters cannot be correlated
  testthat::expect_warning(
    res_cross <- pamlj::pamlmixed(
      aim = "power",
      syntax = "y~1*1+.5*x+.2*z+(a*1*1|cluster)+(b*1*1|cluster2)\ncor(a,b)=.3",
      clusterpars = list(cluster = c(n = 10, k = 20), cluster2 = c(n = 2, k = 10)),
      algo = "raw", verbose = FALSE, .info = TRUE),
    "same cluster")
  testthat::expect_length(res_cross$info$model$random$cluster$covariances, 0)

  ## a symbol attached to a bracket (categorical) random term is not supported
  ## (the design also legitimately triggers pamlj's "partial recycling" notice,
  ## unrelated to this feature, hence suppressWarnings() rather than expect_warning())
  res_bracket <- suppressWarnings(pamlj::pamlmixed(
      aim = "power",
      syntax = "y~1*1+.5*x+[.2,-.2]*grp+(a*1*1+g*[.5,.2]*grp|cluster)\ncor(a,g)=.3",
      clusterpars = cl, categorical = list(grp = 3), algo = "raw", verbose = FALSE, .info = TRUE))
  testthat::expect_length(res_bracket$info$model$random$cluster$covariances, 0)
})


testthat::test_that("pamlmixed(model=) extracts correlated random effects from VarCorr()", {
  testthat::skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  ext <- pamlj:::.mixed_from_fit(fit)
  testthat::expect_true(grepl("cov\\(", ext$syntax))

  ## the round-trip (model= vs. the extracted syntax=) still matches exactly
  ## under the deterministic "raw" algorithm (see the continuous-LMM test above
  ## for why algo="mc" is not compared this way)
  res_model <- pamlj::pamlmixed(model = fit, algo = "raw", set_seed = TRUE, seed = 1, verbose = FALSE)
  res_syn   <- pamlj::pamlmixed(aim = "power", syntax = ext$syntax, clusterpars = ext$clusterpars,
                                 categorical = ext$categorical, sigma2 = ext$sigma2,
                                 model_type = ext$model_type, algo = "raw", set_seed = TRUE, seed = 1,
                                 verbose = FALSE)
  testthat::expect_equal(res_model$powertab$asDF$power, res_syn$powertab$asDF$power)

  ## the extracted covariance matches VarCorr() exactly
  res_info <- pamlj::pamlmixed(model = fit, algo = "raw", set_seed = TRUE, seed = 1, verbose = FALSE, .info = TRUE)
  cv <- res_info$info$model$random$Subject$covariances
  testthat::expect_length(cv, 1)
  testthat::expect_equal(cv[[1]]$cov, as.matrix(lme4::VarCorr(fit)$Subject)[1, 2])
})


testthat::test_that("pamlmixed(model=, focus=) selects a fixed-effect term to test", {
  testthat::skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)

  ext <- pamlj:::.mixed_from_fit(fit, focus = "Days")
  testthat::expect_true(grepl("test: ", ext$syntax))

  ## matches the equivalent hand-built syntax= call with the same `test:` symbol
  res_model <- pamlj::pamlmixed(model = fit, focus = "Days", aim = "n", algo = "raw",
                                 set_seed = TRUE, seed = 1, verbose = FALSE)
  res_syn   <- pamlj::pamlmixed(aim = "n", syntax = ext$syntax, clusterpars = ext$clusterpars,
                                 categorical = ext$categorical, sigma2 = ext$sigma2,
                                 model_type = ext$model_type, algo = "raw", set_seed = TRUE, seed = 1,
                                 verbose = FALSE)
  testthat::expect_equal(res_model$powertab$asDF$power, res_syn$powertab$asDF$power)
  testthat::expect_identical(res_model$powertab$asDF$effect, "Days")

  ## "1"/"(Intercept)" selects the intercept
  ext_int <- pamlj:::.mixed_from_fit(fit, focus = "(Intercept)")
  testthat::expect_true(grepl("test: ", ext_int$syntax))

  ## a categorical (bracket) term can also be the focus
  d <- lme4::sleepstudy
  d$grp <- factor(rep(letters[1:3], length.out = nrow(d)))
  fit2 <- lme4::lmer(Reaction ~ grp + (1 | Subject), data = d)
  ext2 <- pamlj:::.mixed_from_fit(fit2, focus = "grp")
  testthat::expect_true(grepl("test: ", ext2$syntax))
  res2 <- suppressWarnings(pamlj::pamlmixed(model = fit2, focus = "grp", aim = "n", algo = "raw", verbose = FALSE))
  testthat::expect_identical(res2$powertab$asDF$effect, "grp")

  ## an unknown term name fails clearly
  testthat::expect_error(pamlj:::.mixed_from_fit(fit, focus = "Bogus"), "does not match any fixed-effect term")

  ## `focus` without `model` is rejected
  testthat::expect_error(
    pamlj::pamlmixed(syntax = "y~1*1+.5*x+(1*1|cluster)",
                      clusterpars = list(cluster = c(n = 10, k = 20)), focus = "x"),
    "only used together with `model`")
})


testthat::test_that("pamlmixed(model=) handles two crossed clustering factors without a row-count explosion", {
  testthat::skip_if_not_installed("lme4")
  set.seed(1)
  nsubj <- 20; nitem <- 20
  d <- expand.grid(subject = factor(1:nsubj), item = factor(1:nitem))
  d$x <- rnorm(nrow(d))
  d$y <- rbinom(nrow(d), 1, plogis(-2.4 + 0.3 * d$x))
  fit <- suppressMessages(lme4::glmer(y ~ x + (1 | subject) + (1 | item), data = d, family = binomial))

  ext <- pamlj:::.mixed_from_fit(fit)
  ## the shared replication factor sits on the first cluster only; every other
  ## crossed cluster gets n=1, so k_subject * k_item * 1 == nobs (not squared)
  testthat::expect_equal(ext$clusterpars$subject$n * ext$clusterpars$subject$k *
                            ext$clusterpars$item$k, stats::nobs(fit))
  testthat::expect_equal(ext$clusterpars$item$n, 1)

  res <- pamlj::pamlmixed(model = fit, algo = "raw", verbose = FALSE)
  testthat::expect_true(is.finite(res$powertab$asDF$power))
})


testthat::test_that("pamlmixed(model=) auto-detects between-cluster predictors", {
  testthat::skip_if_not_installed("lme4")
  set.seed(1)
  nsubj <- 20; nitem <- 20
  morf_per_item <- factor(sample(c("m1", "m2"), nitem, replace = TRUE))
  cond_per_subj <- factor(sample(c("a", "b"), nsubj, replace = TRUE))
  d <- expand.grid(subject = factor(1:nsubj), item = factor(1:nitem))
  d$morfologia <- morf_per_item[as.integer(d$item)]
  d$cond <- cond_per_subj[as.integer(d$subject)]
  d$y <- rbinom(nrow(d), 1, plogis(-2.4 - 0.3 * (d$cond == "b") + 1.3 * (d$morfologia == "m2")))
  fit <- suppressMessages(lme4::glmer(y ~ cond * morfologia + (1 | subject) + (1 | item),
                                       data = d, family = binomial))

  ext <- pamlj:::.mixed_from_fit(fit)
  testthat::expect_true(grepl("between: cond\\|subject", ext$syntax))
  testthat::expect_true(grepl("between: morfologia\\|item", ext$syntax))
  testthat::expect_true(any(grepl("detected as between-cluster", ext$warnings)))

  ## a predictor that varies freely within every cluster is left unmarked
  d$trial_cond <- factor(sample(c("a", "b"), nrow(d), replace = TRUE))
  fit2 <- suppressMessages(lme4::glmer(y ~ trial_cond + (1 | subject) + (1 | item),
                                        data = d, family = binomial))
  ext2 <- pamlj:::.mixed_from_fit(fit2)
  testthat::expect_false(grepl("between: trial_cond", ext2$syntax))

  ## end to end: this fully-between design runs without error
  res <- pamlj::pamlmixed(model = fit, algo = "raw", verbose = FALSE)
  testthat::expect_true(all(is.finite(res$powertab$asDF$power)))
})


testthat::test_that("pamlmixed(model=) rejects unsupported models", {
  testthat::skip_if_not_installed("lme4")
  testthat::expect_error(pamlj:::.mixed_from_fit(lm(Reaction ~ Days, data = lme4::sleepstudy)),
                          "fitted lme4 model")
  fitp <- suppressMessages(lme4::glmer(
    round(Reaction) ~ Days + (1 | Subject), data = lme4::sleepstudy, family = poisson))
  testthat::expect_error(pamlj:::.mixed_from_fit(fitp), "not supported")
})

