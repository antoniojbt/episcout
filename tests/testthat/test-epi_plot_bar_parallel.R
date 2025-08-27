test_that("epi_plot_bar_list_parallel creates plots", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(
    sex = factor(c("F", "M", "F")),
    group = factor(c("A", "B", "A"))
  )

  plots <- epi_plot_bar_list_parallel(
    vars_to_plot = c("sex", "group"),
    data = df,
    n_cores = 1
  )

  expect_length(plots, 2)
  expect_named(plots, c("sex", "group"))
  lapply(plots, function(p) expect_s3_class(p, "ggplot"))
})

test_that("epi_plot_bar_grid_save writes files", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("cowplot")

  df <- data.frame(sex = factor(c("F", "M")))
  plots <- epi_plot_bar_list_parallel("sex", df, n_cores = 1)

  dir <- tempdir()
  files <- epi_plot_bar_grid_save(
    plot_list = plots,
    results_subdir = dir,
    per_file = 1,
    n_cores = 1
  )

  expect_true(all(file.exists(files)))
})
