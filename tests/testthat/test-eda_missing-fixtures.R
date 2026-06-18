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

test_that("epi_eda_profile_missing counts sentinel missing codes", {
  data <- data.frame(
    age = c(45, 999, NA, 50),
    status = c("A", "UNK", "B", "REF"),
    complete = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    name = c("age", "status", "complete"),
    label = c("Age", "Status", "Complete"),
    type = c("numeric", "categorical", "integer"),
    role = c("covariate", "covariate", "covariate"),
    levels = c("", "A;B;UNK;REF", ""),
    missing_codes = c("999", "UNK; REF", ""),
    stringsAsFactors = FALSE
  )

  observed <- epi_eda_profile_missing(data, spec)

  expect_equal(observed$n, c(4L, 4L, 4L))
  expect_equal(observed$n_missing, c(2L, 2L, 0L))
  expect_equal(observed$p_missing, c(0.5, 0.5, 0))
})

test_that("epi_eda_profile_missing uses NA proportions for zero-row data", {
  data <- data.frame(
    age = numeric(),
    status = character(),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    name = c("age", "status"),
    label = c("Age", "Status"),
    type = c("numeric", "categorical"),
    role = c("covariate", "covariate"),
    levels = c("", "A;B"),
    missing_codes = c("999", ""),
    stringsAsFactors = FALSE
  )

  observed <- epi_eda_profile_missing(data, spec)

  expect_equal(observed$n, c(0L, 0L))
  expect_equal(observed$n_missing, c(0L, 0L))
  expect_true(all(is.na(observed$p_missing)))
})
