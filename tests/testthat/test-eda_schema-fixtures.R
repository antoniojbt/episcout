context("fixture-backed EDA schema tests")

library(testthat)
library(episcout)

fixture_dir <- file.path("fixtures", "blood_storage")
data_path <- file.path(fixture_dir, "blood_storage.csv")
spec_path <- file.path(fixture_dir, "blood_storage_spec.csv")
expected_schema_path <- file.path(fixture_dir, "expected_schema.csv")

test_that("epi_eda_check_schema matches independently computed expected schema", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)
  expected <- read.csv(expected_schema_path, stringsAsFactors = FALSE)

  observed <- epi_eda_check_schema(data, spec)

  expect_equal(as.data.frame(observed), expected, ignore_attr = TRUE)
})

test_that("epi_eda_check_schema flags missing expected variables", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)
  data$Age <- NULL

  observed <- epi_eda_check_schema(data, spec)
  age_row <- observed[observed$name == "Age", ]

  expect_equal(nrow(age_row), 1)
  expect_true(age_row$expected_present)
  expect_false(age_row$observed_present)
  expect_equal(age_row$status, "missing")
})

test_that("epi_eda_check_schema flags unexpected observed variables", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)
  data$UnexpectedVariable <- seq_len(nrow(data))

  observed <- epi_eda_check_schema(data, spec)
  unexpected_row <- observed[observed$name == "UnexpectedVariable", ]

  expect_equal(nrow(unexpected_row), 1)
  expect_false(unexpected_row$expected_present)
  expect_true(unexpected_row$observed_present)
  expect_equal(unexpected_row$status, "unexpected")
})

test_that("all expected variables are present in the unmodified fixture", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)

  observed <- epi_eda_check_schema(data, spec)
  expected_rows <- observed[observed$expected_present, ]

  expect_true(all(expected_rows$observed_present))
  expect_false(any(expected_rows$status == "missing"))
})
