context("fixture-backed epi_eda_run workflow tests")

library(testthat)
library(episcout)

fixture_dir <- file.path("fixtures", "blood_storage")
data_path <- file.path(fixture_dir, "blood_storage.csv")
spec_path <- file.path(fixture_dir, "blood_storage_spec.csv")

expected_components <- c(
  "metadata", "schema", "missing", "geo", "summaries", "categorical_display", "plots",
  "maps", "map_inventory"
)

test_that("epi_eda_run returns expected components for real fixture data", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)

  observed <- epi_eda_run(data = data, spec = spec)

  expect_type(observed, "list")
  expect_named(observed, expected_components)
  expect_s3_class(observed$metadata, "data.frame")
  expect_s3_class(observed$schema, "data.frame")
  expect_s3_class(observed$missing, "data.frame")
  expect_s3_class(observed$geo, "data.frame")
  expect_type(observed$summaries, "list")
  expect_s3_class(observed$categorical_display, "data.frame")
  expect_type(observed$plots, "list")
  expect_type(observed$maps, "list")
  expect_s3_class(observed$map_inventory, "data.frame")
  expect_false(observed$metadata$synthetic)
  expect_identical(observed$metadata$data_origin, "observed")
})

test_that("epi_eda_run real-data workflow matches component functions", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)

  observed <- epi_eda_run(data = data, spec = spec)

  expect_equal(observed$schema, epi_eda_check_schema(data, spec), ignore_attr = TRUE)
  expect_equal(observed$missing, epi_eda_profile_missing(data, spec), ignore_attr = TRUE)
  expect_equal(
    observed$summaries,
    epi_eda_profile_summaries(data, spec),
    tolerance = 1e-12,
    ignore_attr = TRUE
  )
  expect_equal(
    observed$categorical_display,
    epi_eda_categorical_display(observed$summaries),
    tolerance = 1e-12
  )
  expect_named(observed$plots, spec$name)
})

test_that("epi_eda_run supports synthetic fixture workflow", {
  spec <- epi_eda_spec(spec_path)

  observed <- epi_eda_run(
    data = NULL,
    spec = spec,
    synthetic = TRUE,
    n = 25,
    seed = 2024
  )

  expect_type(observed, "list")
  expect_named(observed, expected_components)
  expect_equal(observed$metadata$synthetic, TRUE)
  expect_identical(
    observed$metadata$data_origin,
    "episcout_generated_synthetic"
  )
  expect_equal(observed$metadata$n_rows, 25L)
  expect_equal(observed$schema$status, rep("present", nrow(spec)))
  expect_named(observed$plots, spec$name)
})

test_that("epi_eda_run profiles caller-declared synthetic data unchanged", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)

  observed <- epi_eda_run(
    data = data,
    spec = spec,
    synthetic = FALSE,
    data_origin = "caller_declared_synthetic"
  )

  expect_true(observed$metadata$synthetic)
  expect_identical(observed$metadata$data_origin, "caller_declared_synthetic")
  expect_equal(observed$schema, epi_eda_check_schema(data, spec), ignore_attr = TRUE)
  expect_equal(observed$missing, epi_eda_profile_missing(data, spec), ignore_attr = TRUE)
})

test_that("epi_eda_run rejects contradictory origins before writing", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)
  output_dir <- tempfile("run-eda-contradiction-")
  dir.create(output_dir)

  expect_error(
    epi_eda_run(
      data,
      spec,
      output_dir = output_dir,
      synthetic = TRUE,
      data_origin = "caller_declared_synthetic"
    ),
    "synthetic = TRUE requires"
  )
  expect_length(list.files(output_dir, all.files = TRUE, no.. = TRUE), 0L)

  expect_error(
    epi_eda_run(
      data,
      spec,
      output_dir = output_dir,
      data_origin = "episcout_generated_synthetic"
    ),
    "requires synthetic = TRUE"
  )
  expect_length(list.files(output_dir, all.files = TRUE, no.. = TRUE), 0L)

  expect_error(
    epi_eda_run(data, spec, output_dir = output_dir, data_origin = "synthetic"),
    "data_origin must be"
  )
  expect_length(list.files(output_dir, all.files = TRUE, no.. = TRUE), 0L)

  expect_error(
    epi_eda_run(
      NULL,
      spec,
      output_dir = output_dir,
      data_origin = "caller_declared_synthetic"
    ),
    "requires a supplied data frame"
  )
  expect_length(list.files(output_dir, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("epi_eda_run writes workflow outputs to a temporary output directory", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)
  output_dir <- tempfile("run-eda-fixture-")
  dir.create(output_dir)

  observed <- epi_eda_run(data = data, spec = spec, output_dir = output_dir)

  expect_true(dir.exists(output_dir))
  expect_true(file.exists(file.path(output_dir, "metadata.csv")))
  expect_true(file.exists(file.path(output_dir, "schema.csv")))
  expect_true(file.exists(file.path(output_dir, "missing.csv")))
  expect_true(file.exists(file.path(output_dir, "geo_qa.csv")))
  expect_true(file.exists(file.path(output_dir, "summary_numeric.csv")))
  expect_true(file.exists(file.path(output_dir, "summary_categorical.csv")))
  expect_true(file.exists(file.path(output_dir, "categorical_display.csv")))
  expect_true(file.exists(file.path(output_dir, "summary_variables.csv")))
  expect_true(file.exists(file.path(output_dir, "summary_text.csv")))
  expect_true(file.exists(file.path(output_dir, "summary_temporal.csv")))
  expect_true(file.exists(file.path(output_dir, "summary_skipped.csv")))
  expect_equal(
    names(utils::read.csv(file.path(output_dir, "schema.csv"), check.names = FALSE)),
    names(observed$schema)
  )
})
