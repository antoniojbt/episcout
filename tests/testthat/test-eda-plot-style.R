context("EDA creation-time plot styling")

test_that("plot_style receives compact context and transforms returned plots", {
  skip_if_not_installed("ggplot2")
  data <- data.frame(category = factor(c("low", "high", "low"), levels = c("low", "medium", "high")))
  spec <- data.frame(
    name = "category", label = "Category", database_type = "text", analysis_type = "categorical",
    role = "measure", levels = "low;medium;high", stringsAsFactors = FALSE
  )
  seen <- new.env(parent = emptyenv())
  seen$context <- NULL
  style <- function(plot, context) {
    seen$context <- context
    plot + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))
  }
  styled <- epi_eda_profile_plots(data, spec, plot_style = style)

  expect_s3_class(styled$category, "ggplot")
  expect_identical(names(seen$context), c("name", "label", "type", "plot_type", "n_total", "n_missing", "n_plotted", "n_excluded_non_finite"))
  expect_identical(seen$context$name, "category")
  expect_identical(seen$context$plot_type, "frequency")
  expect_false("data" %in% names(seen$context))
  expect_false("connection" %in% names(seen$context))
})

test_that("plot_style failures are actionable and do not accept non-plots", {
  data <- data.frame(value = c(1, 2, 3))
  spec <- data.frame(name = "value", label = "Value", database_type = "text", analysis_type = "numeric", role = "measure", stringsAsFactors = FALSE)
  expect_error(epi_eda_profile_plots(data, spec, plot_style = "style"), "NULL or a function")
  expect_error(epi_eda_profile_plots(data, spec, plot_style = function(plot, context) "not a plot"), "must return")
  expect_error(epi_eda_profile_plots(data, spec, plot_style = function(plot, context) stop("failure")), "plot_style failed")
})

test_that("epi_eda_run routes plot_style without changing analytical summaries", {
  data <- data.frame(value = c(1, 2, 3))
  spec <- data.frame(name = "value", label = "Value", database_type = "text", analysis_type = "numeric", role = "measure", stringsAsFactors = FALSE)
  plain <- epi_eda_run(data, spec)
  styled <- epi_eda_run(data, spec, plot_style = function(plot, context) plot + ggplot2::theme_minimal())
  expect_identical(styled$summaries, plain$summaries)
  expect_s3_class(styled$plots$value, "ggplot")
})

test_that("database style provenance is validated before database work", {
  source <- structure(list(), class = c("epi_eda_postgres_source", "list"))
  expect_error(
    epi_eda_db_run(source, data.frame(), tempfile("style-id-"), plot_style = function(plot, context) plot),
    "plot_style_id is required"
  )
  expect_error(
    epi_eda_db_run(source, data.frame(), tempfile("style-id-"), plot_style_id = "style-v1"),
    "requires plot_style"
  )
})
