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
  expect_match(template_text, "plot|Plot")
})

test_that("render_eda_report renders a real fixture-data report", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- eda_spec(spec_path)
  output_dir <- tempfile("eda-report-real-")
  dir.create(output_dir)

  report_path <- render_eda_report(
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

test_that("render_eda_report labels synthetic fixture reports", {
  spec <- eda_spec(spec_path)
  output_dir <- tempfile("eda-report-synthetic-")
  dir.create(output_dir)

  report_path <- render_eda_report(
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

test_that("render_eda_report writes machine-readable workflow outputs", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- eda_spec(spec_path)
  output_dir <- tempfile("eda-report-outputs-")
  dir.create(output_dir)

  render_eda_report(
    data = data,
    spec = spec,
    output_dir = output_dir
  )

  expect_true(file.exists(file.path(output_dir, "metadata.csv")))
  expect_true(file.exists(file.path(output_dir, "schema.csv")))
  expect_true(file.exists(file.path(output_dir, "missing.csv")))
  expect_true(file.exists(file.path(output_dir, "summary_numeric.csv")))
  expect_true(file.exists(file.path(output_dir, "summary_categorical.csv")))
})

test_that("render_eda_report requires an existing output directory", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- eda_spec(spec_path)
  output_dir <- tempfile("eda-report-missing-")

  expect_error(
    render_eda_report(
      data = data,
      spec = spec,
      output_dir = output_dir
    ),
    regexp = "output_dir|directory|exist"
  )
})
