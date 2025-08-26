test_that("epi_plot_km returns ggplot and data matches summary", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")

  fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = survival::lung)
  plt <- epi_plot_km(fit)
  expect_s3_class(plt, "ggplot")

  surv_data <- attr(plt, "surv_data")
  sum_fit <- summary(fit)
  expected <- tibble::tibble(
    time = sum_fit$time,
    surv = sum_fit$surv,
    n_risk = sum_fit$n.risk,
    n_event = sum_fit$n.event,
    n_censor = sum_fit$n.censor,
    strata = if (is.null(sum_fit$strata)) "All" else as.character(sum_fit$strata)
  )
  expect_equal(surv_data, expected)
})
