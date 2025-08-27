context("parallel plot functions")

skip_if_not_installed("future")
skip_if_not_installed("foreach")
skip_if_not_installed("ggplot2")
skip_if_not_installed("cowplot")

skip_if(parallel::detectCores() < 2, "Not enough cores")

library(episcout)
library(ggplot2)

# Data for tests
mt <- mtcars

print("Testing epi_plot_parallel")

test_that("epi_plot_parallel generates plots using multiple workers", {
  plots <- epi_plot_parallel(mt,
    vars_to_plot = c("mpg", "disp"),
    num_cores = 2,
    future_plan = "multicore"
  )
  expect_equal(length(plots), 2)
  expect_equal(sort(names(plots)), sort(c("mpg", "disp")))
  expect_gt(attr(plots, "workers"), 1)
})

print("Testing epi_plot_save_parallel")

test_that("epi_plot_save_parallel saves plots in parallel", {
  plots <- list(mpg = epi_plot_hist(mt, "mpg"), disp = epi_plot_hist(mt, "disp"))
  tmp_prefix <- file.path(tempdir(), "plots_parallel")
  files <- epi_plot_save_parallel(
    plots,
    file_prefix = tmp_prefix,
    plot_type = "png",
    plot_step = 1,
    num_cores = 2,
    future_plan = "multicore"
  )
  expect_equal(length(files), 2)
  expect_true(all(file.exists(files)))
  expect_gt(attr(files, "workers"), 1)
})
