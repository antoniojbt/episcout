context("fixture-backed EDA schema tests")

library(testthat)
library(episcout)

fixture_dir <- file.path("fixtures", "blood_storage")
data_path <- file.path(fixture_dir, "blood_storage.csv")
spec_path <- file.path(fixture_dir, "blood_storage_spec.csv")
expected_schema_path <- file.path(fixture_dir, "expected_schema.csv")

test_that("epi_eda_check_schema preserves the historical fixture schema projection", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)
  expected <- read.csv(expected_schema_path, stringsAsFactors = FALSE)

  observed <- epi_eda_check_schema(data, spec)

  expect_equal(observed[names(expected)], expected, ignore_attr = TRUE)
  expect_named(observed, c(names(expected), "type_status", "type_reason"))
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
  expect_equal(age_row$type_status, "not_applicable")
  expect_match(age_row$type_reason, "not present")
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
  expect_equal(unexpected_row$type_status, "not_applicable")
  expect_match(unexpected_row$type_reason, "not declared")
})

test_that("all expected variables are present in the unmodified fixture", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)

  observed <- epi_eda_check_schema(data, spec)
  expected_rows <- observed[observed$expected_present, ]

  expect_true(all(expected_rows$observed_present))
  expect_false(any(expected_rows$status == "missing"))
})

test_that("epi_eda_check_schema remains descriptive for numeric and binary types", {
  data <- data.frame(
    integer_count = c(1L, 2L),
    logical_flag = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    name = c("integer_count", "logical_flag"),
    label = c("Integer count", "Logical flag"),
    database_type = "text", analysis_type = c("integer", "binary"),
    role = c("covariate", "covariate"),
    stringsAsFactors = FALSE
  )

  observed <- epi_eda_check_schema(data, spec)

  expect_equal(observed$expected_type, c("integer", "binary"))
  expect_equal(observed$observed_type, c("numeric", "binary"))
  expect_equal(observed$status, c("present", "present"))
})

test_that("epi_eda_check_schema reports compatibility independently of presence", {
  data <- data.frame(
    numeric_double = c(1.5, 2.5),
    numeric_integer = c(1L, 2L),
    integer_integer = c(1L, 2L),
    integer_whole_double = c(1, 2),
    integer_fractional = c(1, 2.5),
    category_factor = factor(c("a", "b")),
    category_character = c("a", "b"),
    category_codes = c(1, 2),
    category_bad_codes = c(1, 4),
    binary_logical = c(TRUE, FALSE),
    binary_codes = c(0, 1),
    binary_bad_codes = c(0, 2),
    text_character = c("alpha", "beta"),
    text_factor = factor(c("alpha", "beta")),
    date_native = as.Date(c("2024-01-01", "2024-01-02")),
    date_iso = c("2024-01-01", "2024-01-02"),
    date_invalid = c("2024-01-01", "not-a-date"),
    datetime_native = as.POSIXct(c("2024-01-01 12:00:00", "2024-01-02 12:00:00"), tz = "UTC"),
    datetime_iso = c("2024-01-01T12:00:00Z", "2024-01-02T13:30:00-05:30"),
    datetime_invalid = c("2024-01-01T12:00:00Z", "not-a-datetime"),
    stringsAsFactors = FALSE
  )
  data$unsupported_list <- I(list(1, 2))
  data$unexpected_column <- c("kept", "visible")
  expected_names <- c(
    "numeric_double", "numeric_integer", "integer_integer", "integer_whole_double", "integer_fractional",
    "category_factor", "category_character", "category_codes", "category_bad_codes",
    "binary_logical", "binary_codes", "binary_bad_codes", "text_character", "text_factor",
    "date_native", "date_iso", "date_invalid", "datetime_native", "datetime_iso", "datetime_invalid",
    "unsupported_list", "missing_variable"
  )
  spec <- data.frame(
    name = expected_names,
    label = expected_names,
    database_type = "text", analysis_type = c(
      "numeric", "numeric", "integer", "integer", "integer",
      "categorical", "categorical", "categorical", "categorical",
      "binary", "binary", "binary", "text", "text",
      "date", "date", "date", "datetime", "datetime", "datetime",
      "text", "numeric"
    ),
    role = rep("covariate", length(expected_names)),
    levels = c(
      rep("", 7), "1;2;3", "1;2;3", "TRUE;FALSE", "0;1", "0;1",
      rep("", 10)
    ),
    stringsAsFactors = FALSE
  )
  expected_type_status <- c(
    "compatible", "compatible", "compatible", "coercible", "incompatible",
    "compatible", "compatible", "coercible", "incompatible",
    "compatible", "coercible", "incompatible", "compatible", "coercible",
    "compatible", "coercible", "incompatible", "compatible", "coercible", "incompatible",
    "incompatible", "not_applicable", "not_applicable"
  )

  observed <- epi_eda_check_schema(data, spec)

  expect_identical(observed$name, c(expected_names, "unexpected_column"))
  expect_identical(observed$type_status, expected_type_status)
  expect_true(all(nzchar(observed$type_reason)))
  expect_match(observed$type_reason[observed$name == "integer_whole_double"], "whole numbers")
  expect_match(observed$type_reason[observed$name == "date_iso"], "ISO dates")
  expect_match(observed$type_reason[observed$name == "datetime_iso"], "ISO-8601")
  expect_match(observed$type_reason[observed$name == "unsupported_list"], "AsIs")
})

test_that("schema compatibility ignores declared missing codes", {
  data <- data.frame(
    coded_category = c(1, 2, 999),
    coded_binary = c("yes", "no", "UNK"),
    coded_date = c("2024-01-01", "2024-01-02", "MISSING"),
    coded_datetime = c("2024-01-01T12:00:00Z", "2024-01-02T12:00:00Z", "MISSING"),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    name = names(data),
    label = names(data),
    database_type = "text", analysis_type = c("categorical", "binary", "date", "datetime"),
    role = rep("covariate", 4),
    levels = c("1;2", "yes;no", "", ""),
    missing_codes = c("999", "UNK", "MISSING", "MISSING"),
    stringsAsFactors = FALSE
  )

  observed <- epi_eda_check_schema(data, spec)

  expect_identical(observed$type_status, rep("coercible", 4))
})
