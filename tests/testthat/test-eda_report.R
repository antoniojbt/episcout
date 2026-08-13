context("fixture-backed EDA report-rendering tests")

library(testthat)
library(episcout)

fixture_dir <- file.path("fixtures", "blood_storage")
data_path <- file.path(fixture_dir, "blood_storage.csv")
spec_path <- file.path(fixture_dir, "blood_storage_spec.csv")

read_report_text <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

expect_report_file <- function(path) {
  expect_type(path, "character")
  expect_length(path, 1L)
  expect_true(file.exists(path))
  expect_match(basename(path), "\\.html?$")
}

test_that("EDA report template is bundled with report sections", {
  template_path <- system.file(
    "report-template",
    "eda.qmd",
    package = "episcout"
  )

  expect_true(nzchar(template_path))
  if (!nzchar(template_path)) {
    return(invisible())
  }

  template_text <- read_report_text(template_path)
  expect_match(template_text, "schema|Schema")
  expect_match(template_text, "missing|Missing")
  expect_match(template_text, "summar|Summar")
  expect_match(template_text, "categorical numerator|Categorical numerator")
  expect_match(template_text, "plot|Plot")
  expect_match(template_text, "map|Map")
  expect_match(
    template_text,
    "episcout creates the outputs explicitly requested by the analyst",
    fixed = TRUE
  )
})

test_that("epi_eda_render_report renders a real fixture-data report", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)
  output_dir <- tempfile("eda-report-real-")
  dir.create(output_dir)

  report_path <- epi_eda_render_report(
    data = data,
    spec = spec,
    output_dir = output_dir
  )

  expect_report_file(report_path)

  report_text <- read_report_text(report_path)
  expect_match(report_text, "schema|Schema")
  expect_match(report_text, "missing|Missing")
  expect_match(report_text, "summar|Summar")
  expect_match(report_text, "plot|Plot")
  expect_match(report_text, "real|Real")
})

test_that("epi_eda_render_report labels synthetic fixture reports", {
  spec <- epi_eda_spec(spec_path)
  output_dir <- tempfile("eda-report-synthetic-")
  dir.create(output_dir)

  report_path <- epi_eda_render_report(
    data = NULL,
    spec = spec,
    output_dir = output_dir,
    synthetic = TRUE,
    n = 25,
    seed = 2024
  )

  expect_report_file(report_path)
  expect_match(read_report_text(report_path), "synthetic|Synthetic")
})

test_that("epi_eda_render_report writes machine-readable workflow outputs", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)
  output_dir <- tempfile("eda-report-outputs-")
  dir.create(output_dir)

  epi_eda_render_report(
    data = data,
    spec = spec,
    output_dir = output_dir
  )

  expect_true(file.exists(file.path(output_dir, "metadata.csv")))
  expect_true(file.exists(file.path(output_dir, "schema.csv")))
  expect_true(file.exists(file.path(output_dir, "missing.csv")))
  expect_true(file.exists(file.path(output_dir, "summary_numeric.csv")))
  expect_true(file.exists(file.path(output_dir, "summary_categorical.csv")))
  expect_true(file.exists(file.path(output_dir, "categorical_display.csv")))
})

test_that("epi_eda_render_report renders complete canonical summary sections", {
  data <- data.frame(
    value = c(1, 2),
    note = c("x", " "),
    observed = as.Date(c("2020-01-01", "2020-01-02")),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    name = c("value", "note", "observed"),
    label = c("Value", "Note", "Observed"),
    database_type = "text", analysis_type = c("numeric", "text", "date"),
    role = c("covariate", "metadata", "covariate")
  )
  output_dir <- tempfile("eda-report-canonical-")
  dir.create(output_dir)

  report_path <- epi_eda_render_report(
    data = data,
    spec = spec,
    output_dir = output_dir
  )
  report_text <- read_report_text(report_path)

  expect_match(report_text, "Variables Summaries")
  expect_match(report_text, "Numeric Summaries")
  expect_match(report_text, "Text Summaries")
  expect_match(report_text, "Temporal Summaries")
  expect_match(report_text, "Skipped Summaries")
  expect_match(report_text, "No skipped rows")
  expect_match(report_text, "finite observed values")
  expect_match(report_text, "p_total")
  expect_match(report_text, "range_unit")
  expect_true(file.exists(file.path(output_dir, "summary_variables.csv")))
  expect_true(file.exists(file.path(output_dir, "summary_skipped.csv")))
})

test_that("epi_eda_render_report requires an existing output directory", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)
  output_dir <- tempfile("eda-report-missing-")

  expect_error(
    epi_eda_render_report(
      data = data,
      spec = spec,
      output_dir = output_dir
    ),
    regexp = "output_dir|directory|exist"
  )
})

test_that("epi_eda_render_report embeds declared point maps", {
  data <- data.frame(
    lon = c(-10, 0, 10),
    lat = c(-5, 0, 5),
    theme = c("A", "MISSING", "B"),
    stringsAsFactors = FALSE
  )
  spec <- epi_eda_spec_scaffold(data)
  spec$geo_role[1:2] <- c("x", "y")
  spec$geo_pair[1:2] <- "site"
  spec$geo_crs[1:2] <- "4326"
  spec$missing_codes[[3]] <- "MISSING"
  output_dir <- tempfile("eda-report-maps-")
  dir.create(output_dir)

  report_path <- epi_eda_render_report(
    data,
    spec,
    output_dir,
    maps = TRUE,
    map_vars = "theme"
  )

  report_text <- read_report_text(report_path)
  expect_match(report_text, "Map inventory", fixed = TRUE)
  expect_match(report_text, "map-p001-geometry", fixed = TRUE)
  expect_match(report_text, "map-p001-v003", fixed = TRUE)
  expect_true(file.exists(file.path(output_dir, "maps", "map-p001-geometry.svg")))
  expect_true(file.exists(file.path(output_dir, "maps", "map-p001-v003.svg")))
})
