context("epi_plot_heatmap_triangle value selection")

library(episcout)
library(testthat)

skip_if_not_installed("Hmisc")
skip_if_not_installed("ggplot2")

set.seed(1)
mini_df <- data.frame(
  A = 1:5,
  B = rnorm(5),
  C = rpois(5, 2)
)

corr_res <- epi_stats_corr(mini_df, method = "spearman")
tri <- epi_stats_corr_triangle(corr_res$cormat)

plot_corr <- epi_plot_heatmap_triangle(
  tri$cormat_melted_triangle_r,
  tri$cormat_melted_triangle_pval,
  cor_method = "Spearman",
  show_values = "corr"
)

plot_pval <- epi_plot_heatmap_triangle(
  tri$cormat_melted_triangle_r,
  tri$cormat_melted_triangle_pval,
  cor_method = "Spearman",
  show_values = "pval"
)

test_that("geom_text uses correlation data when show_values = 'corr'", {
  expect_equal(plot_corr$layers[[2]]$data, tri$cormat_melted_triangle_r)
})

test_that("geom_text uses p-value data when show_values = 'pval'", {
  expect_equal(plot_pval$layers[[2]]$data, tri$cormat_melted_triangle_pval)
})

test_that("legend title includes correlation method", {
  expect_true(grepl("Spearman", plot_corr$scales$scales[[1]]$name))
  expect_true(grepl("Spearman", plot_pval$scales$scales[[1]]$name))
})
