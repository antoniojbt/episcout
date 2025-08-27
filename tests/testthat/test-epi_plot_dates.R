library(testthat)

skip_if_not_installed("ggplot2")
skip_if_not_installed("ggthemes")

sample_dates <- as.Date("2020-01-01") + 0:9

test_that("histogram plot is created", {
  p <- epi_plot_dates(sample_dates, type = "hist")
  expect_s3_class(p, "ggplot")
  layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
  expect_true("GeomBar" %in% layer_classes)
})

test_that("boxplot is created", {
  p <- epi_plot_dates(sample_dates, type = "box")
  expect_s3_class(p, "ggplot")
  layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
  expect_true("GeomBoxplot" %in% layer_classes)
})

test_that("line plot is created", {
  p <- epi_plot_dates(sample_dates, type = "line")
  expect_s3_class(p, "ggplot")
  layer_classes <- sapply(p$layers, function(x) class(x$geom)[1])
  expect_true(all(c("GeomLine", "GeomPoint") %in% layer_classes))
})
