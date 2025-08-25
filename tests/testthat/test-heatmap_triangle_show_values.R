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

plot_no_labels <- epi_plot_heatmap_triangle(
  tri$cormat_melted_triangle_r,
  tri$cormat_melted_triangle_pval,
  cor_method = "Spearman",
  show_values = "pval",
  show_labels = FALSE
)

test_that("geom_text uses correlation data when show_values = 'corr'", {
  geom_text_layer_index <- which(vapply(plot_corr$layers, function(layer) "GeomText" %in% class(layer$geom), logical(1)))
  expect_equal(plot_corr$layers[[geom_text_layer_index]]$data, tri$cormat_melted_triangle_r)
})

test_that("geom_text uses p-value data when show_values = 'pval'", {
  geom_text_layer_index <- which(vapply(plot_pval$layers, function(layer) "GeomText" %in% class(layer$geom), logical(1)))
  expect_equal(plot_pval$layers[[geom_text_layer_index]]$data, tri$cormat_melted_triangle_pval)
})

test_that("legend title includes correlation method", {
  expect_true(any(grepl("Spearman", vapply(plot_corr$scales$scales, function(scale) scale$name, character(1)))))
  expect_true(any(grepl("Spearman", vapply(plot_pval$scales$scales, function(scale) scale$name, character(1)))))
})

test_that("labels are omitted when show_labels = FALSE", {
  has_text <- any(vapply(plot_no_labels$layers, function(layer) "GeomText" %in% class(layer$geom), logical(1)))
  expect_false(has_text)
})
