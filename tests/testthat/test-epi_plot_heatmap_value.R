context("epi_plot_heatmap value handling")

library(episcout)
library(testthat)

# small helper data frames
corr_df <- data.frame(
  Var1 = "a",
  Var2 = "b",
  correlation = 0.5
)

pval_df <- data.frame(
  Var1 = "a",
  Var2 = "b",
  pvalue = 0.1
)

no_val_df <- data.frame(
  Var1 = "a",
  Var2 = "b"
)


test_that("correlation column is used as value", {
  p <- epi_plot_heatmap(corr_df, title = "Custom")
  expect_s3_class(p, "ggplot")
  expect_true(all(p$data$value == corr_df$correlation))
  expect_equal(p$labels$title, "Custom")
})


test_that("pvalue column is used as value", {
  p <- epi_plot_heatmap(pval_df)
  expect_true(all(p$data$value == pval_df$pvalue))
})


test_that("error when no value column", {
  expect_error(
    epi_plot_heatmap(no_val_df),
    "Column `value`, `correlation` or `pvalue` not found"
  )
})
