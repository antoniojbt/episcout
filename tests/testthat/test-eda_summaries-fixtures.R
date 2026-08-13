context("fixture-backed EDA summary tests")

library(testthat)
library(episcout)

fixture_dir <- file.path("fixtures", "blood_storage")
data_path <- file.path(fixture_dir, "blood_storage.csv")
spec_path <- file.path(fixture_dir, "blood_storage_spec.csv")

test_that("epi_eda_profile_summaries returns all canonical summary components", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)

  observed <- epi_eda_profile_summaries(data, spec)

  expect_type(observed, "list")
  expect_named(observed, c("variables", "numeric", "categorical", "text", "temporal", "skipped"))
  expect_s3_class(observed$variables, "data.frame")
  expect_s3_class(observed$numeric, "data.frame")
  expect_s3_class(observed$categorical, "data.frame")
  expect_s3_class(observed$text, "data.frame")
  expect_s3_class(observed$temporal, "data.frame")
  expect_s3_class(observed$skipped, "data.frame")
})

test_that("epi_eda_profile_summaries numeric output matches hand-computed values", {
  data <- data.frame(
    age = c(10, 20, 999, NA),
    all_missing = c(999, NA, 999, NA),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    name = c("age", "all_missing"),
    label = c("Age", "All missing"),
    database_type = "text", analysis_type = c("numeric", "numeric"),
    role = c("covariate", "covariate"),
    missing_codes = c("999", "999"),
    stringsAsFactors = FALSE
  )
  expected <- data.frame(
    name = c("age", "all_missing"),
    mean = c(15, NA_real_),
    sd = c(sqrt(50), NA_real_),
    median = c(15, NA_real_),
    min = c(10, NA_real_),
    max = c(20, NA_real_),
    stringsAsFactors = FALSE
  )

  observed <- epi_eda_profile_summaries(data, spec)

  expect_equal(
    as.data.frame(observed$numeric[c("name", "mean", "sd", "median", "min", "max")]),
    expected,
    tolerance = 1e-12,
    ignore_attr = TRUE
  )
})

test_that("epi_eda_profile_summaries categorical output documents denominators", {
  data <- data.frame(
    status = c("A", "B", "UNK", NA, "A"),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    name = "status",
    label = "Status",
    database_type = "text", analysis_type = "categorical",
    role = "covariate",
    levels = "A;B;C",
    missing_codes = "UNK",
    stringsAsFactors = FALSE
  )
  expected <- data.frame(
    name = c("status", "status", "status"),
    level = c("A", "B", "C"),
    n = c(2L, 1L, 0L),
    p_total = c(2 / 5, 1 / 5, 0),
    p_observed = c(2 / 3, 1 / 3, 0),
    is_declared = c(TRUE, TRUE, TRUE),
    is_unexpected = c(FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  observed <- epi_eda_profile_summaries(data, spec)

  expect_equal(
    as.data.frame(observed$categorical),
    expected,
    tolerance = 1e-12,
    ignore_attr = TRUE
  )
})

test_that("blood storage canonical summaries cover every specified variable", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)

  observed <- epi_eda_profile_summaries(data, spec)

  expect_equal(observed$variables$name, spec$name)
  expect_true(all(observed$variables$status == "summarised"))
  expect_equal(nrow(observed$skipped), 0L)
  expect_setequal(observed$numeric$name, spec$name[spec$analysis_type %in% c("numeric", "integer")])
  expect_setequal(unique(observed$categorical$name), spec$name[spec$analysis_type %in% c("categorical", "binary")])
})
