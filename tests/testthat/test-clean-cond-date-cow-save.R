context("additional tests for epi_clean_cond_date and epi_plot_cow_save")

library(episcout)
library(testthat)

# epi_clean_cond_date ------------------------------------------------------

skip_if_not_installed("lubridate")

# Test identification of Date and POSIXct columns

test_that("epi_clean_cond_date identifies date-like objects", {
  date_vec <- as.Date("2020-01-01") + 0:2
  posix_vec <- as.POSIXct(date_vec)
  num_vec <- 1:3

  expect_true(epi_clean_cond_date(date_vec))
  expect_true(epi_clean_cond_date(posix_vec))
  expect_false(epi_clean_cond_date(num_vec))
})

# epi_plot_cow_save --------------------------------------------------------

skip_if_not_installed("cowplot")
skip_if_not_installed("ggplot2")

# Test saving single plot and grid of plots

test_that("epi_plot_cow_save handles ggplot and grid inputs", {
  tmp_single <- tempfile(fileext = ".pdf")
  tmp_multi  <- tempfile(fileext = ".pdf")

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
    ggplot2::geom_point()

  expect_message(
    epi_plot_cow_save(file_name = tmp_single, plot_grid = p),
    "Saving a single ggplot object"
  )
  expect_true(file.exists(tmp_single))

  plot_list <- list(p, p)
  expect_message(
    epi_plot_cow_save(file_name = tmp_multi, plot_grid = plot_list),
    "Saving a multi-plot grid"
  )
  expect_true(file.exists(tmp_multi))

  unlink(c(tmp_single, tmp_multi))
})
