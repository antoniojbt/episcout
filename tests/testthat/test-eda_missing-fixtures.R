context("fixture-backed EDA missingness tests")

library(testthat)
library(episcout)

fixture_dir <- file.path("fixtures", "blood_storage")
data_path <- file.path(fixture_dir, "blood_storage.csv")
spec_path <- file.path(fixture_dir, "blood_storage_spec.csv")
expected_missing_path <- file.path(fixture_dir, "expected_missing.csv")

test_that("epi_eda_profile_missing matches independently computed expected missingness", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)
  expected <- read.csv(expected_missing_path, stringsAsFactors = FALSE)

  observed <- epi_eda_profile_missing(data, spec)

  expect_equal(
    as.data.frame(observed),
    expected,
    tolerance = 1e-12,
    ignore_attr = TRUE
  )
})

test_that("epi_eda_profile_missing reports the correct denominator for every variable", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)

  observed <- epi_eda_profile_missing(data, spec)

  expect_true(all(observed$n == nrow(data)))
})

test_that("epi_eda_profile_missing percentages are stable with tolerance", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)
  expected <- read.csv(expected_missing_path, stringsAsFactors = FALSE)

  observed <- epi_eda_profile_missing(data, spec)

  expect_equal(observed$p_missing, expected$p_missing, tolerance = 1e-12)
})

test_that("epi_eda_profile_missing reports zero missingness for complete variables", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)
  expected <- read.csv(expected_missing_path, stringsAsFactors = FALSE)
  complete_names <- expected$name[expected$n_missing == 0]

  observed <- epi_eda_profile_missing(data, spec)
  complete_rows <- observed[observed$name %in% complete_names, ]

  expect_true(all(complete_rows$n_missing == 0))
  expect_true(all(complete_rows$p_missing == 0))
})
