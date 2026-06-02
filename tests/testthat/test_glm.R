testthat::context("glm")

with_fake_pamlglm_class <- function(code) {
  ns <- asNamespace("pamlj")
  original <- get("pamlglmClass", envir = ns)
  captured <- new.env(parent = emptyenv())
  result <- list(ok = TRUE)

  fake_class <- list(
    new = function(options, data) {
      captured$options <- options
      captured$data <- data
      list(
        run = function() {
          captured$ran <- TRUE
          invisible(NULL)
        },
        results = result
      )
    }
  )

  unlockBinding("pamlglmClass", ns)
  assign("pamlglmClass", fake_class, envir = ns)
  lockBinding("pamlglmClass", ns)

  on.exit({
    unlockBinding("pamlglmClass", ns)
    assign("pamlglmClass", original, envir = ns)
    lockBinding("pamlglmClass", ns)
  }, add = TRUE)

  force(code)(captured, result)
}


testthat::test_that("pamlglm formals do not expose internal arguments", {
  fmls <- names(formals(pamlj::pamlglm))

  testthat::expect_false(".caller" %in% fmls)
  testthat::expect_false(".interface" %in% fmls)
})


testthat::test_that("pamlglm rejects .interface as a public argument", {
  testthat::expect_error(
    pamlj::pamlglm(data = data.frame(x = 1:3), .interface = "jamovi"),
    "unused argument"
  )
})


testthat::test_that("pamlglm rejects .caller as a public argument", {
  testthat::expect_error(
    pamlj::pamlglm(data = data.frame(x = 1:3), .caller = "other"),
    "unused argument"
  )
})


testthat::test_that("pamlglm injects fixed R interface values into options", {
 
    out <- pamlj::pamlglm(
      aim = "power",
      mode = "beta",
      es = 0.3,
      r2 = 0.10,
      df_model = 2,
      n = 50
    )

    testthat::expect_identical(out, result)
    testthat::expect_true(isTRUE(captured$ran))
    testthat::expect_identical(captured$options$.interface, "R")
    testthat::expect_identical(captured$options$.caller, "glm")
 })


testthat::test_that("pamlglm forwards data and user arguments to options", {

    pamlj::pamlglm(
      aim = "power",
      mode = "beta",
      es = 0.2,
      r2 = 0.12,
      df_model = 3,
      n = 20,
      alternative = "one.sided"
    )

    testthat::expect_identical(captured$data, dat)
    testthat::expect_identical(captured$options$aim, "power")
    testthat::expect_identical(captured$options$mode, "beta")
    testthat::expect_equal(captured$options$b_es, 0.25)
    testthat::expect_equal(captured$options$b_r2, 0.12)
    testthat::expect_equal(captured$options$b_df_model, 3)
    testthat::expect_equal(captured$options$power, 0.85)
    testthat::expect_equal(captured$options$n, 64)
    testthat::expect_identical(captured$options$alternative, "one.sided")
})


testthat::test_that("pamlglm maps peta arguments onto peta options", {
  with_fake_pamlglm_class(function(captured, result) {
    pamlj::pamlglm(
      data = data.frame(y = 1:4, x = 2:5),
      mode = "peta",
      es = 0.18,
      df_model = 4,
      df_effect = 2
    )

    testthat::expect_equal(captured$options$v_es, 0.18)
    testthat::expect_equal(captured$options$v_df_model, 4)
    testthat::expect_equal(captured$options$v_df_effect, 2)
  })
})


testthat::test_that("pamlglm maps eta arguments onto eta options", {
  with_fake_pamlglm_class(function(captured, result) {
    pamlj::pamlglm(
      data = data.frame(y = 1:4, x = 2:5),
      mode = "eta",
      es = 0.22,
      df_model = 5,
      df_effect = 3,
      r2 = 0.40
    )

    testthat::expect_equal(captured$options$e_es, 0.22)
    testthat::expect_equal(captured$options$e_df_model, 5)
    testthat::expect_equal(captured$options$e_df_effect, 3)
    testthat::expect_equal(captured$options$e_r2, 0.40)
  })
})
