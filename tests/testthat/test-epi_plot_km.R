library(testthat)
library(episcout)

test_that("epi_survfit_to_df converts survfit objects", {
  skip_if_not_installed("survival")
  fit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
  df <- epi_survfit_to_df(fit)
  expect_true(is.data.frame(df))
  expect_true(all(c("time", "surv") %in% names(df)))
  expect_equal(nrow(df), length(fit$time))
})

test_that("epi_plot_km creates plot and saves to file", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")
  fit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
  tmp <- tempfile(fileext = ".png")
  p <- epi_plot_km(fit, group_var = "sex", save_path = tmp)
  expect_true(inherits(p, "ggplot"))
  expect_true(file.exists(tmp))
})
