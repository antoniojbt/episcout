context("fixture-backed run_eda workflow tests")

library(testthat)
library(episcout)

fixture_dir <- file.path("fixtures", "blood_storage")
data_path <- file.path(fixture_dir, "blood_storage.csv")
spec_path <- file.path(fixture_dir, "blood_storage_spec.csv")

expected_components <- c("metadata", "schema", "missing", "summaries", "plots")

test_that("run_eda returns expected components for real fixture data", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- eda_spec(spec_path)

  observed <- run_eda(data = data, spec = spec)

  expect_type(observed, "list")
  expect_named(observed, expected_components)
  expect_s3_class(observed$metadata, "data.frame")
  expect_s3_class(observed$schema, "data.frame")
  expect_s3_class(observed$missing, "data.frame")
  expect_type(observed$summaries, "list")
  expect_type(observed$plots, "list")
})

test_that("run_eda real-data workflow matches component functions", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- eda_spec(spec_path)

  observed <- run_eda(data = data, spec = spec)

  expect_equal(observed$schema, check_schema(data, spec), ignore_attr = TRUE)
  expect_equal(observed$missing, profile_missing(data, spec), ignore_attr = TRUE)
  expect_equal(
    observed$summaries,
    profile_summaries(data, spec),
    tolerance = 1e-12,
    ignore_attr = TRUE
  )
  expect_named(observed$plots, spec$name)
})

test_that("run_eda supports synthetic fixture workflow", {
  spec <- eda_spec(spec_path)

  observed <- run_eda(
    data = NULL,
    spec = spec,
    synthetic = TRUE,
    n = 25,
    seed = 2024
  )

  expect_type(observed, "list")
  expect_named(observed, expected_components)
  expect_equal(observed$metadata$synthetic, TRUE)
  expect_equal(observed$metadata$n_rows, 25L)
  expect_equal(observed$schema$status, rep("present", nrow(spec)))
  expect_named(observed$plots, spec$name)
})

test_that("run_eda writes workflow outputs to a temporary output directory", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- eda_spec(spec_path)
  output_dir <- tempfile("run-eda-fixture-")
  dir.create(output_dir)

  observed <- run_eda(data = data, spec = spec, output_dir = output_dir)

  expect_true(dir.exists(output_dir))
  expect_true(file.exists(file.path(output_dir, "metadata.csv")))
  expect_true(file.exists(file.path(output_dir, "schema.csv")))
  expect_true(file.exists(file.path(output_dir, "missing.csv")))
  expect_true(file.exists(file.path(output_dir, "summary_numeric.csv")))
  expect_true(file.exists(file.path(output_dir, "summary_categorical.csv")))
  expect_equal(
    names(utils::read.csv(file.path(output_dir, "schema.csv"), check.names = FALSE)),
    names(observed$schema)
  )
})
