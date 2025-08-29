context("migrated_plots")

######################
# Tests for functions migrated from legacy
######################

set.seed(1)

# epi_plot_grid_size ---------------------------------------------------------

test_that("epi_plot_grid_size calculates grid dimensions", {
  plots <- list(a = 1, b = 2, c = 3, d = 4)
  grid_size <- epi_plot_grid_size(plots)
  expect_equal(grid_size$ncol_grid, 2)
  expect_equal(grid_size$nrow_grid, 2)
})

# epi_plot_volcano -----------------------------------------------------------

test_that("epi_plot_volcano runs without error", {
  skip_on_ci()
  logFC <- rnorm(10)
  p_val <- runif(10)
  expect_null(epi_plot_volcano(logFC, p_val))
})
