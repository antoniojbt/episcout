context("epi_plot_themes")

library(episcout)
library(testthat)

skip_if_not_installed("ggplot2")
skip_if_not_installed("ggthemes")
skip_if_not_installed("scales")

# Test epi_plot_theme_1 default output

test_that("epi_plot_theme_1 returns a theme", {
  thm <- epi_plot_theme_1()
  expect_s3_class(thm, "theme")
  expect_equal(thm$plot.title$face, "bold")
})

# Test epi_plot_theme_2 with custom axis text sizes

test_that("epi_plot_theme_2 handles font sizes", {
  thm <- epi_plot_theme_2(font_size_x = 5, font_size_y = 6)
  expect_s3_class(thm, "theme")
  expect_equal(thm$axis.text.x$size, 5)
  expect_equal(thm$axis.text.y$size, 6)
})

# Test palette scales

test_that("scale_fill_epi_plot_theme_2 returns manual scale", {
  sc <- scale_fill_epi_plot_theme_2()
  expect_true(inherits(sc, "ScaleDiscrete"))
  cols <- sc$palette(9)
  expect_equal(cols[1], "#386cb0")
  expect_equal(length(cols), 9)
})


test_that("scale_colour_epi_plot_theme_2 returns manual scale", {
  sc <- scale_colour_epi_plot_theme_2()
  expect_true(inherits(sc, "ScaleDiscrete"))
  cols <- sc$palette(9)
  expect_equal(cols[2], "#fdb462")
  expect_equal(length(cols), 9)
})

